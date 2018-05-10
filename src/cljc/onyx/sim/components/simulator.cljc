(ns onyx.sim.components.simulator
  (:require 
    [com.stuartsierra.component :as component]
    [taoensso.timbre :as log]
    [datascript.core :as d]
    #?(:cljs [onyx.sim.ui :as ui])
    [clojure.core.async :as async :refer [go go-loop <! >!]]
    [onyx.sim.api :as api]))

; (defn- create-conn []
;   {:dat.sync.db/transact! d/transact!
;    :dat.sync.db/q d/q
;    :dat.sync.db/pull d/pull
;    :dat.sync.db/deref deref
;    :dat.sync.db/conn (d/create-conn schema)})

(defn gen-uuid []
  (d/squuid))

(defn- simplify [sim-job]
  (-> (dissoc sim-job :onyx/type :onyx/doc :onyx/name)
      (clojure.set/rename-keys {:onyx.core/catalog         :catalog
                                :onyx.core/workflow        :workflow
                                :onyx.core/lifecycles      :lifecycles
                                :onyx.core/windows         :windows
                                :onyx.core/triggers        :triggers
                                :onyx.core/task-scheduler  :task-scheduler
                                :onyc.core/metadata        :metadata
                                :onyx.core/flow-conditions :flow-conditions})))

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

(defn submit-job [{:keys [conn envs]} job]
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
    (d/transact!
      conn
      [(assoc job :onyx/job-id job-id)])
    true))

(defrecord OnyxSimulator [envs control> conn]
  component/Lifecycle
  (start [component]
    ;; FIXME: ui/schema is hacked in here due to lacking update schema for datascript
    (let [conn (or conn (d/create-conn (api/idents->schema (concat api/schema-idents #?(:cljs ui/schema-idents)))))
          simulator
          {:control> (or control> (async/chan))
           :envs     (or envs (atom {}))
           :conn conn}]
      (go-schedule-jobs! simulator)
      (into component simulator)))
  (stop [component]
    (go (>! control> ::kill))
    (assoc component
      :conn     nil
      :envs     nil
      :control> nil)))

(defn new-onyx-sim 
  ([]
   (new-onyx-sim {}))
  ([opts]
   (map->OnyxSimulator opts)))
