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

;; ???: Can onyx sim have a resource map that allows it to inject resources into incoming segments and/or the lifecycle map? Maybe render segment takes in resources?
;; ???: Should there be some kind of asynchronous job runner that caches compiled jobs? If so how do we get async to interface with react?

(defn show-ui [conn]
  (let [debug true]
    (if debug
      [sim/sim-selector conn]
      [:div
;;        [:p "/todo"]
;;        [dat.view/render-segment
;;         {:dat.sync.db/conn conn
;;          :onyx.sim/sim [:onyx/name :dat.view/sim]
;;          :dat.view/route :dat.view.route/todos}]
;;        [dat.view/render-segment
;;         {:dat.sync.db/conn conn
;;          :onyx.sim/sim [:onyx/name :dat.view/sim]
;;          :dat.view/route :dat.view.route/todo
;;          :dat.view/entity 28}]
;;        [sim/sim-selector ^{:dat.sync.db/conn conn} {:dat.sync.db/conn conn}]

       ])))

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
