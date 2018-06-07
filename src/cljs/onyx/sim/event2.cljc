(ns onyx.sim.event2
  (:require
    [onyx.sim.kb :as kb]
    [onyx.sim.sub :as sub]))
  
(defn update-eav! [{:keys [knowbase]} eid attr value]
  (kb/transact!
    knowbase
    (fn [kbs]
      {:db [[:db/add eid attr value]]})))
  
(defn update-setting! [sim attr value]
  (update-eav! sim [:onyx.sim.ui/name :onyx.sim.ui/settings] attr value))
  
(defn hide-task! [{:keys [knowbase]} task-name]
  (kb/transact!
    knowbase
    (fn [kbs]
      (let [{:onyx.sim.ui/keys [hidden-tasks]} (kb/q kbs sub/?hidden-tasks)]
        {:db [[:db/add 
               [:onyx/job-id (:onyx.sim.ui/job-id kbs)]
               :onyx.sim.ui/hidden-tasks
               (conj (or hidden-tasks #{}) task-name)]]}))))
