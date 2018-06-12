(ns onyx.sim.components.sim
  (:require
    [com.stuartsierra.component :as component]
    [taoensso.timbre :as log]
    [onyx.sim.api :as api]
    [datascript.core :as d]
    [onyx.sim.utils :refer [gen-uuid]]
    [clojure.core.async :as async :refer [go go-loop <! >!]]
    #?(:cljs [reagent.core :refer [atom]])))

(defn go-envs! [envs tss> & {:as opts :keys [control>]}]
  ; (log/info "go-envs!")
  (let [control> (or control> (async/chan))]
    (go-loop []
      (let [[tss-or-sig ch] (async/alts! [control> tss>])]
        ; (log/info "received tss" (:onyx.sim.api/event tss-or-sig))
        (if (= ch control>)
          (if (= tss-or-sig ::kill)
            (log/info "Kill Signal recieved. Closing job scheduler...")
            (do 
              (log/warn "Unhandled signal:" tss-or-sig)
              (recur)))
          (let [{:as tss :onyx/keys [job-id] :onyx.sim.api/keys [event]} tss-or-sig
                ; _ (log/info "tssing" job-id)
                env (get @envs job-id)
                ; _ (log/info "env retrieved" (get-in env [:tasks :in :inbox]) (get-in env [:tasks :out :outputs]))
                env-after (api/<transition-env env tss)]
            (swap! envs assoc job-id env-after)
            (when (and (= event :onyx.sim.api/go-step)
                       (not= (api/status env-after) :onyx.sim.api/polls-closed))
              ;; FIXME: use different marker for continuous transitions
              (go (>! tss> tss)))
              
            (recur)))))
    control>))

(defn submit-job [{:keys [tss>]} job]
  (let [job-id (or (:onyx/job-id job) (gen-uuid))]
    (log/info "Submitting " job-id)
    (go (>! tss> {:onyx.sim.api/event :onyx.sim.api/init
                  :onyx/job-id job-id
                  :onyx.sim.api/job job})
        (>! tss> {:onyx.sim.api/event :onyx.sim.api/go-step
                  :onyx/job-id job-id}))))

(defn transition! [{:as sim :keys [tss>]} tss]
  (go (>! tss> tss)))

(defn kill! [{:keys [control>]}]
  (when control>
    (go (>! control> ::kill))))

(defrecord OnyxSimulator [envs tss> control>]
  ;; ???: give tss> a buffer. May need to be flushed on stop.
  component/Lifecycle
  (start [component]
    (let [tss> (or tss> (async/chan))
          envs (or envs (atom {}))]
      (assoc
        component
        :tss> tss>
        :envs envs
        :control> (or control> (go-envs! envs tss>)))))
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
