(ns onyx.sim.lifecycle
  (:require [taoensso.timbre :as log]))

(defn inject-resources [event {::keys [inject resources] :as lifecycle}]
  (into
    {}
    (for [[k path] inject]
      (do 
        ; (log/info "k path" k path)
        [k (get-in resources path)]))))

(def resource-calls 
  {:lifecycle/before-task-start inject-resources})

(defn bind-resources [job resources]
  (update
    job
    :lifecycles
    (fn [lifecycles]
      (mapv #(assoc % ::resources resources) lifecycles))))
