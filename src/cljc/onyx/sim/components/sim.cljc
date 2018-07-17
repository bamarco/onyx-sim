(ns onyx.sim.components.sim
  (:require
    [com.stuartsierra.component :as component]
    [taoensso.timbre :as log]
    ; #?(:clj [clj-datetime :as time]
    ;    :cljs [cljs-datetime :as time])
    [datascript.core :as d]
    [onyx.sim.utils :refer [gen-uuid <apply go-let controlled-loop]]
    [onyx.sim.api :as api]
    [clojure.core.async :as async :refer [go go-loop <! >!]]
    #?(:cljs [reagent.core :refer [atom]])))

(defn- fire-transition? [now {:as tss :keys [last-fired frequency]}]
  (= now (+ last-fired frequency)))

(defn- min-timeout [state]
  (let [now nil;(time/now)
        tsses (get @state ::recurring-tsses)]
    (transduce
      (map 
        (fn [{:keys [frequency last-fired]}]
          (- frequency (- now last-fired))))
      min
      tsses)))

(defn- tss!> [state tss]
  (let [env-id (:onyx/job-id tss)
        state* @state
        env (get-in state* [::envs env-id])]
    (go-let [env-after (api/transition-env env tss)]
      ;; TODO: if recurring tss update last-fired time
      (when-not (compare-and-set! state state* (assoc-in state* [::envs env-id] env-after))
        ;; FIXME: Not quite correct on the exception logic. Adding new envs should not be a problem, but currently will race.
        (throw (ex-info "The envs-atom should only be set from the go-envs! loop. Race conditions may occur. Failed to process tss." {:tss tss}))))))

(defn- fire-recurring!> [state]
  (let [now nil;;(time/now)
        state* @state
        tsses (get state* ::recurring-tsses)]
    (go
      (doseq [tss (filter (partial fire-transition? now) tsses)]
        (<apply tss!> state tss)))))

(defn- handle-tsses!> [state tss> & {:as opts :keys [control>]}]
  (controlled-loop control>
    (let [timeout (min-timeout state)
          _ (log/info "tsses timeout" timeout)
          timeout> (when timeout (async/timeout timeout))
          [seg ch] (async/alts! (into [] (remove nil?) [control> tss> timeout>]))]
      (condp = ch
        control>
        (case seg
          ::kill (log/info "Kill Signal received. Closing job scheduler...")
          (do (log/warn "Unhandled signal:" seg) (recur)))

        tss>
        (do
          (log/info "Handles tss" seg) 
          (<apply tss!> state seg) (recur))

        timeout>
        (do 
          (log/info "Handle timeout")
          (<apply fire-recurring!> state) (recur))))))

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
          state (or state (atom {::envs {} ::recurring-tsses []}))]
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
