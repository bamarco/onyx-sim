(ns onyx.sim.components.ui
  (:require [com.stuartsierra.component :as component]
            [taoensso.timbre :as log]
            [reagent.core :as reagent]
            [onyx.sim.ui :as sim.ui]
            [onyx.sim.dat-view :as dat.view]
            [onyx.sim.core :as sim]))

(defn show-ui [sim]
  [sim.ui/selector sim])

(defrecord UIComponent [knowbase simulator]
  component/Lifecycle
  (start
    [component]
    (reagent/render [show-ui simulator] (js/document.getElementById "app"))
    component)
  (stop [component]
    component))

(defn new-ui-component []
  (map->UIComponent {}))
