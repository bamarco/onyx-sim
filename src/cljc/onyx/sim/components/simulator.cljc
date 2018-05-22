(ns onyx.sim.components.simulator
  (:require 
    [com.stuartsierra.component :as component]
    [taoensso.timbre :as log]
    [datascript.core :as d]
    [onyx.sim.kb :as kb]
    #?(:cljs [posh.reagent :as posh])
    #?(:cljs [onyx.sim.ui :as ui])
    #?(:cljs [reagent.core :as r])
    [clojure.core.async :as async :refer [go go-loop <! >!]]
    [onyx.sim.api :as api]))

(defn gen-uuid []
  (d/squuid))

(defn- simplify [sim-job]
  (-> sim-job
      (clojure.set/rename-keys {:onyx.core/catalog         :catalog
                                :onyx.core/workflow        :workflow
                                :onyx.core/lifecycles      :lifecycles
                                :onyx.core/windows         :windows
                                :onyx.core/triggers        :triggers
                                :onyx.core/task-scheduler  :task-scheduler
                                :onyc.core/metadata        :metadata
                                :onyx.core/flow-conditions :flow-conditions})
      (select-keys [:catalog :workflow :lifecycles :windows :triggers :task-scheduler :metadata :flow-conditions])))

(defn into-envs [envs-before envs-after]
  (into
    envs-before
    (for [env envs-after]
      [(:onyx.sim.api/job-id env) env])))

(defn go-schedule-jobs! [{:keys [envs control>]}]
  ;; TODO: scheduler
  (go-loop []
    (let [envs-before (vec (remove :onyx.sim.api/completed? (vals @envs)))
          _ (log/debug "Scheduling jobs:" (mapv :onyx.sim.api/job-id envs-before))
          [envs-or-signal ch] (async/alts! [(api/go-envs! envs-before) control>])]
      (if (= ch control>)
        (if (= envs-or-signal ::kill)
          (log/info "Kill Signal recieved. Closing job scheduler...")
          (do 
            (log/warn "Unhandled signal:" envs-or-signal)
            (recur)))
        (let [pause? (every?
                      #(= (:onyx.sim.api/signal %) :onyx.sim.api/pause)
                      envs-or-signal)
              envs-after (map api/mark-completed envs-or-signal)]
          (if pause?
            (do
              (log/debug "Nothing to poll, pausing now")
              (<! (async/timeout 100))
              (swap! envs into-envs (map api/clear-signals envs-after)))
            (do
              (log/debug "Envs processed successfully")
              (swap! envs into-envs envs-after)))
          (recur))))))

(defn submit-job [{:keys [knowbase envs]} job]
  (let [job-id (or (:onyx/job-id job) (gen-uuid))]
    ;; TODO: make idempotent based on job-id/job-hash
    (log/info "Submitting job:" job-id)
    (swap!
      envs
      assoc
      job-id
      (into
        (api/init (simplify job))
        {:onyx.sim.api/job-id job-id
         :onyx.sim.api/complete? false}))
    (kb/transact!
      knowbase
      (fn [kbs]
        {:db [(assoc job :onyx/job-id job-id)]}))
    true))

(defrecord OnyxSimulator [knowbase envs control> event> job-id]
  component/Lifecycle
  (start [component]
    ;; FIXME: ui/schema is hacked in here due to lacking update schema for datascript
    (let [sim
          (assoc
            component
            :control> (or control> (async/chan))
            :event>   (or event> (async/chan))
            :envs     (or envs (#?(:clj atom :cljs r/atom) {})))]
      ;; FIXME: make idempotent
      (go-schedule-jobs! sim)
      sim))
  (stop [component]
    (go (>! control> ::kill))
    (assoc component
      :envs nil
      :control> nil
      :event> nil
      :job-id nil)))

(defn new-onyx-sim 
  ([]
   (new-onyx-sim {}))
  ([opts]
   (map->OnyxSimulator opts)))
