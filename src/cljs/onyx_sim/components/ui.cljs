(ns onyx-sim.components.ui
  (:require [com.stuartsierra.component :as component]
            [onyx-sim.core :refer [render]]))

(defrecord UIComponent []
  component/Lifecycle
  (start [component]
    (render)
    component)
  (stop [component]
    component))

(defn new-ui-component []
  (map->UIComponent {}))
