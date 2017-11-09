(ns onyx.sim.components.ui
  (:require [com.stuartsierra.component :as component]
            [taoensso.timbre :as log]
            [reagent.core :as reagent]
            [posh.reagent :as p]
            [onyx.sim.core :as sim]
            [onyx.sim.api :as onyx]
            [datascript.core :as ds]
            [onyx.sim.dat-view :as dat.view]
            [dat.sync.db :as d]))

(defn show-ui [conn]
  (let [debug true]
    (if debug
      [sim/sim-selector {:dat.sync.db/conn conn}]
      [:div
       [dat.view/render-segment
        {:dat.sync.db/conn conn
         :onyx.sim/sim [:onyx/name :dat.view/sim]
         :dat.view/route :dat.view.route/todos}]
       [dat.view/render-segment
        {:dat.sync.db/conn conn
         :onyx.sim/sim [:onyx/name :dat.view/sim]
         :dat.view/route :dat.view.route/index}]])))

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
