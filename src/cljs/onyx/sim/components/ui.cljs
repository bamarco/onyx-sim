(ns onyx.sim.components.ui
  (:require [com.stuartsierra.component :as component]
            [taoensso.timbre :as log]
            [reagent.core :as reagent]
            [onyx.sim.dat-view :as dat.view]
            [onyx.sim.core :as sim]))

(defn show-ui [conn]
  (let [debug true
        sys {:dat.sync.db/conn conn
             :onyx.sim/sim [:onyx/name :dat.view/sim]}]
    (if debug
      [sim/sim-selector conn]
      [:div
       [:p "Rendering /todo"]
       [dat.view/render-segment
        sys
        {:dat.view/route :dat.view.route/todos}]
        [sim/sim-selector conn]])))

(defrecord UIComponent [knowbase]
  component/Lifecycle
  (start
    [component]
      (reagent/render [show-ui (:conn knowbase)] (js/document.getElementById "app"))
    component)
  (stop [component]
    component))

(defn new-ui-component []
  (map->UIComponent {}))
