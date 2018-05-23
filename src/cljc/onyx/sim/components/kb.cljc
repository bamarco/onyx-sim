(ns onyx.sim.components.kb
  (:require 
    [com.stuartsierra.component :as component]))

(defrecord KnowledgeBase []
  component/Lifecycle
  (start [component]
    component)
  (stop [component]
    component))

(defn new-knowledge-base 
  ([] (new-knowledge-base {}))
  ([opts] (map->KnowledgeBase opts)))
