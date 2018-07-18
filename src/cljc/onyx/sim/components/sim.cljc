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

(defn- fire-transition? [now {:as tss ::keys [last-fired frequency]}]
  (= now (+ last-fired frequency)))

(defn- next-fire [{::keys [frequency last-fired]}]
  (when (and last-fired frequency)
    (time/plus last-fired frequency)))

(defn- min-time
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

(defn- min-timeout [state*]
  (let [now (time/now)
        tsses (vals (get state* ::recurring-tsses))
        next-fire (transduce (map next-fire) min-time tsses)]  
    (when next-fire
      (time/in-millis (time/interval now next-fire)))))

(defn- update-fired-tss [state* {:as tss ::keys [tss-id recurring? last-fired]}]
  (let [now (time/now)]
    (if recurring?
      (-> state*
        (assoc-in [::recurring-tsses tss-id] tss)
        (assoc-in [::recurring-tsses tss-id ::last-fired] now))
      state*)))

(defn- tss!> [state* tss]
  (let [env-id (:onyx/job-id tss)
        env (get-in state* [::envs env-id])]
    (go-let [env-after (api/transition-env env tss)]
      (-> state*
        (update-fired-tss tss)
        (assoc-in [::envs env-id] env-after)))))

(defn- fire-recurring!> [state*]
  (let [now (time/now)
        tsses (filter (partial fire-transition? now) (get state* ::recurring-tsses))]
    (go-loop [state* state*
              tsses (vals tsses)]
      (let [tss (first tsses)]
        (if-not tss state*
          (let [state-after* (<apply tss!> state* tss)]
            (recur state-after* (rest tsses))))))))

(defn- handle-tsses!> [state tss> & {:as opts :keys [control>]}]
  (controlled-loop control>
    (let [state* @state
          timeout (min-timeout state*)
          _ (log/info "tsses timeout" timeout)
          timeout> (when timeout (async/timeout timeout))
          [seg ch] (async/alts! (into [] (remove nil?) [control> tss> timeout>]))]
      (condp = ch
        control>
        (case seg
          ::kill (log/info "Kill Signal received. Closing job scheduler...")
          (do (log/warn "Unhandled signal:" seg) (recur)))

        tss>
        (let [state-after* (<apply tss!> state* seg)]
          (log/info "Handle tss" seg)
          (if (compare-and-set! state state* state-after*)
            (recur)
            (throw (ex-info "The envs-atom should only be set from the handle-tsses! loop. Race conditions may occur. Failed to process tss" {:tss seg}))))

        timeout>
        (let [state-after* (<apply fire-recurring!> state)]
          (log/info "Handle timeout")
          (if (compare-and-set! state state* state-after*)
            (recur)
            (throw (ex-info "The envs-atom should only be set from the handle-tsses! loop. Race conditions may occur" {}))))))))

; (defn go-envs! [envs tss> & {:as opts :keys [control>]}]
;   ; (log/info "go-envs!")
;   (let [control> (or control> (async/chan))]
;     (go-loop []
;       (let [[tss-or-sig ch] (async/alts! [control> tss>])]
;         ; (log/info "received tss" (:onyx.sim.api/event tss-or-sig))
;         (if (= ch control>)
;           (if (= tss-or-sig ::kill)
;             (log/info "Kill Signal recieved. Closing job scheduler...")
;             (do 
;               (log/warn "Unhandled signal:" tss-or-sig)
;               (recur)))
;           (let [{:as tss :onyx/keys [job-id] :onyx.sim.api/keys [event]} tss-or-sig
;                 _ (log/info "tssing" job-id)
;                 env (get @envs job-id)
;                 ; _ (log/info "env retrieved" (get-in env [:tasks :in :inbox]) (get-in env [:tasks :out :outputs]))
;                 env-after (<apply api/transition-env env tss)]
;             (swap! envs assoc job-id env-after)
;             (when (and (= event :onyx.sim.api/go-step)
;                        (not= (api/status env-after) :onyx.sim.api/polls-closed))
;               ;; FIXME: use different marker for continuous transitions
;               (go (>! tss> tss)))
              
;             (recur)))))
;     control>))

(defn submit-job [{:keys [tss>]} job]
  (let [job-id (or (:onyx/job-id job) (gen-uuid))]
    (log/info "Submitting job #" job-id)
    (go (>! tss> {:onyx.sim.api/event :onyx.sim.api/init
                  :onyx/job-id job-id
                  :onyx.sim.api/job (assoc job :onyx/job-id job-id)}))))

(defn transition! [{:as sim :keys [tss>]} tss]
  (go (>! tss> tss)))

(defn kill! [{:keys [control>]}]
  (when control>
    (go (>! control> ::kill))))

;
(defn env-in
  ([{:keys [state]}] (::envs @state))
  ([sim path] (get-in (env-in sim) path)))

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
        :control> (or control> (handle-tsses!> state tss>)))))
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
