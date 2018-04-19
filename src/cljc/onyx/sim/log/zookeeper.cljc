(ns onyx.sim.log.zookeeper
  (:require [com.stuartsierra.component :as component]
            [taoensso.timbre :refer [info debug]]
            [onyx.extensions :as extensions]
            [datascript.core :as d]
            [clojure.core.async :refer [go >! <!]]))

(def entry-schema {})
(def entry-query '[:find ?entry .
                   :where
                   [?entry ::message-id]])

(defrecord SimZooKeeper [config entries]
  component/Lifecycle
    (start [component]
      (assoc component :entries (or entries (d/create-conn entry-schema))))
    (stop [component]
      ;; ???: Getting rid of all listeners may be too aggressive here.
      (reset! (:listeners (meta entries)) {})
      component))

(defn sim-zookeeper 
  ([] 
   (sim-zookeeper {}))
  ([m]
   (map->SimZooKeeper m)))

(defmethod extensions/write-log-entry SimZooKeeper
  [{:as log :keys [entries]} data]
  (d/transact!
    entries
    [:db.fn/call]
    (fn [db]
      (let [entries (d/q entry-query db)] 
        (assoc data ::message-id (count entries)))))
  log)

(defmethod extensions/read-log-entry SimZooKeeper
  [{:keys [entries]} n]
  (d/pull @entries '[*] [::message-id n]))

(defmethod extensions/register-pulse SimZooKeeper
  [& all])

(defmethod extensions/on-delete SimZooKeeper
  [& all])

(defmethod extensions/group-exists? SimZooKeeper
  [& all]
    ;; Always show true - we will always manually leave
  true)

; (defmethod extensions/subscribe-to-log SimZooKeeper
;   [{:keys [entries]} ch]
;   (listen!
;     entries
;     ch
;     (fn [{:keys [entries]}]
;       (when-let [last-entry (last entries)]
;         (go
;           (>! ch last-entry))))))

; (defmethod extensions/write-chunk :default
;     [log kw chunk id]
;     (cond 
;      (= :task kw)
;      (swap! (:store log) assoc [kw id (:id chunk)] chunk)
;      (= :exception kw)
;      (do (info "Task Exception:" chunk)
;          (throw chunk))
;      :else
;      (swap! (:store log) assoc [kw id] chunk))
;     log)

; (defmethod extensions/read-chunk :default
;     [log kw id & rst]
;     (if (= :task kw)
;       (get @(:store log) [kw id (first rst)])
;       (get @(:store log) [kw id])))

; (defmethod checkpoint/write-checkpoint SimZooKeeper
;     [log tenancy-id job-id replica-version epoch task-id slot-id checkpoint-type checkpoint]
;     (info "Writing checkpoint:" replica-version epoch task-id slot-id)
;     (swap! (:checkpoints log)
;            assoc-in 
;            [tenancy-id :checkpoints job-id [replica-version epoch] [task-id slot-id checkpoint-type]]
;            checkpoint))

; (defmethod checkpoint/complete? SimZooKeeper
;     [_]
;     ;; synchronous write means it's already completed
;     true)

; (defmethod checkpoint/cancel! SimZooKeeper
;     [_])
  
; (defmethod checkpoint/stop SimZooKeeper
;     [log] 
;     ;; zookeeper connection is shared with peer group, so we don't want to stop it
;     log)

; (defmethod checkpoint/write-checkpoint-coordinate SimZooKeeper
;     [log tenancy-id job-id coordinate version]
;     (let [path [tenancy-id :latest job-id]]
;       (-> (swap! (:checkpoints log) 
;                  update-in 
;                  path
;                  (fn [v]
;                    (if (= (or (:version v) 0) version)
;                      {:version (inc version)
;                       :coordinate coordinate}
;                      (throw (KeeperException$BadVersionException. 
;                              (str "failed write " version " vs " (:version v)))))))
  
;           (get-in path))))

; (defmethod checkpoint/assume-checkpoint-coordinate SimZooKeeper
;     [log tenancy-id job-id]
;     (let [exists (get-in @(:checkpoints log) [tenancy-id :latest job-id])
;           version (get exists :version 0)
;           coordinate (get exists :coordinate)]
;       (:version (checkpoint/write-checkpoint-coordinate log tenancy-id job-id 
;                                                         coordinate version))))

; (defmethod checkpoint/read-checkpoint-coordinate SimZooKeeper
;     [log tenancy-id job-id]
;     (get-in @(:checkpoints log) [tenancy-id :latest job-id :coordinate]))

; (defmethod checkpoint/read-checkpoint SimZooKeeper
;     [log tenancy-id job-id replica-version epoch task-id slot-id checkpoint-type]
;     (-> @(:checkpoints log) 
;         (get tenancy-id)
;         :checkpoints
;         (get job-id)
;         (get [replica-version epoch])
;         (get [task-id slot-id checkpoint-type])))
  
