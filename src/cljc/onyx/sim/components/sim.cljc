(ns onyx.sim.components.sim
  (:require
    [com.stuartsierra.component :as component]
    [taoensso.timbre :as log]
    #?(:clj [clj-time.core :as time]
       :cljs [cljs-time.core :as time])
    [datascript.core :as d]
    [onyx.sim.utils :refer [gen-uuid <apply go-let controlled-loop]]
    [onyx.sim.api :as api]
    [clojure.core.async :as async :refer [go go-loop <! >!]]
    #?(:cljs [reagent.core :refer [atom]])))

(def default-timeout-secs 6)

(defn- frequency [{:as tss ::keys [frequency]}]
  (or frequency (time/seconds default-timeout-secs)))

(defn recurring-tsses [state*]
  ; (log/info "recurring-tsses")
  (vals (::recurring-tsses state*)))

(defn- next-fire-time [{:as tss ::keys [last-fired]}]
  (if-not last-fired (time/now)
    (time/plus last-fired (frequency tss))))

(defn fire-transition? [now tss]
  ; (log/info "fire-tss?" now tss)
  (let [next-fire (next-fire-time tss)]
    (or 
      (time/before? next-fire now)
      (time/equal? next-fire now))))

(defn- ensure-tss-id [tss]
  (update tss ::tss-id #(or % (gen-uuid))))

(defn min-time
  ([] nil)
  ([lowest-time] lowest-time)
  ([lowest-time another-time]
   (cond
     (not lowest-time)
     another-time

     (not another-time)
     lowest-time

     :else
     (if (time/before? lowest-time another-time)
       lowest-time
       another-time))))

(defn- next-fire-timeout [state*]
  (let [now (time/now)
        ; _ (log/info "finding timeout" now)
        tsses (recurring-tsses state*)
        ; _ (log/info "recurring tsses" tsses)
        next-fire (transduce (map next-fire-time) min-time tsses)]
    ; (log/info "next-fire" next-fire)
    (when next-fire
      (time/in-millis (time/interval now next-fire)))))

(defn- update-fired-tss [state* {:as tss ::keys [tss-id recurring? suspend-recurring? last-fired]}]
  (let [now (time/now)]
    (if recurring?
      (-> state*
        (assoc-in [::recurring-tsses tss-id] tss)
        (assoc-in [::recurring-tsses tss-id ::last-fired] now))
      state*)))

(defn- update-recurring [state* {:as tss ::keys [tss-id suspend-recurring?]}]
  (if suspend-recurring?
    (update state* ::recurring-tsses dissoc tss-id)
    state*))

(defn- tss!> [state* tss]
  (let [env-id (:onyx/job-id tss)
        env (get-in state* [::envs env-id])]
    ; (log/info "tss!>")
    (go-let [env-after (api/transition-env env tss)]
      ; (log/info "env-after" env-after)
      (-> state*
        (update-fired-tss tss)
        (update-recurring tss)
        (assoc-in [::envs env-id] env-after)))))

(defn- fire-recurring!> [state*]
  ; (log/info "fire-recurring!>")
  (let [now (time/now)
        tsses (filter (partial fire-transition? now) (recurring-tsses state*))]
    (go-loop [state* state*
              tsses tsses]
      (let [tss (first tsses)]
        ; (log/info "recurring state*" state* tss)
        (if-not tss state*
          (let [state-after* (<apply tss!> state* tss)]
            (recur state-after* (rest tsses))))))))

(defn- handle-transitions!> [state tss> & {:as opts :keys [control>]}]
  (controlled-loop control>
    (let [state* @state
          ; _ (log/info "no fit state" state*)
          timeout (next-fire-timeout state*)
          ; _ (log/info "tsses timeout" timeout)
          timeout> (when timeout (async/timeout timeout))
          [seg ch] (async/alts! (into [] (remove nil?) [control> tss> timeout>]))]
      (condp = ch
        control>
        (case seg
          ::kill (log/info "Kill Signal received. Closing job scheduler...")
          (do (log/warn "Unhandled signal:" seg) (recur)))

        tss>
        (let [state-after* (<apply tss!> state* seg)]
          ; (log/info "Handle tss" seg)
          ; (log/info "state-after*" state-after*)
          (if (compare-and-set! state state* state-after*)
            (recur)
            (throw (ex-info "The envs-atom should only be set from the handle-transitions! loop. Race conditions may occur. Failed to process tss" {:tss seg}))))

        timeout>
        (let [state-after* (<apply fire-recurring!> state*)]
          ; (log/info "Handle timeout")
          ; (log/info "state-after*" (::recurring-tsses state-after*))
          (if (compare-and-set! state state* state-after*)
            (recur)
            (throw (ex-info "The envs-atom should only be set from the handle-transitions! loop. Race conditions may occur" {}))))))))

(defn submit-job [{:keys [tss>]} job]
  (let [job-id (or (:onyx/job-id job) (gen-uuid))]
    (log/info "Submitting job #" job-id)
    (go (>! tss> {:onyx.sim.api/event :onyx.sim.api/init
                  :onyx/job-id job-id
                  :onyx.sim.api/job (assoc job :onyx/job-id job-id)}))))

(defn transition! [{:as sim :keys [tss>]} tss]
  (go (>! tss> (ensure-tss-id tss))))

(defn kill! [{:keys [control>]}]
  (when control>
    (go (>! control> ::kill))))

(defn env-in
  ([{:keys [state]}] (::envs @state))
  ([sim path] (get-in (env-in sim) path)))

(defn recurring-transitions-for
  ([{:keys [state]}] (recurring-tsses @state))
  ([sim job-id]
   (log/info "rec tss for" job-id)
   (filter #(= job-id (:onyx/job-id %)) (recurring-transitions-for sim))))

(defrecord OnyxSimulator [state tss> control>]
  ;; ???: give tss> a buffer. May need to be flushed on stop.
  component/Lifecycle
  (start [component]
    (let [tss> (or tss> (async/chan))
          state (or state (atom {::envs {} ::recurring-tsses {}}))]
      (assoc
        component
        :tss> tss>
        :state state
        :control> (or control> (handle-transitions!> state tss>)))))
  (stop [component]
    (kill! component)
    (assoc component
      :control> nil
      :envs nil
      :tss> nil)))

(defn new-sim
  ([]
   (new-sim {}))
  ([opts]
   (map->OnyxSimulator opts)))
