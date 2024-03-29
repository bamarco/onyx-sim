(ns onyx.sim.components.ui
  (:require [com.stuartsierra.component :as component]
            [taoensso.timbre :as log]
            [reagent.dom :refer [render]]

            [onyx.sim.dat-view :as dat.view]
            [onyx.sim.core :as sim]))

(defn show-ui [conn]
  [sim/sim-selector conn])

(defrecord UIComponent [knowbase]
  component/Lifecycle
  (start
    [component]
      (render [show-ui (:conn knowbase)] (js/document.getElementById "app"))
    component)
  (stop [component]
    component))

(defn new-ui-component []
  (map->UIComponent {}))
