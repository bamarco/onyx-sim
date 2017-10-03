(ns onyx.sim.components.ui
  (:require [com.stuartsierra.component :as component]
            [reagent.core :as reagent]
            [posh.reagent :as p]
            [onyx.sim.core :as sim]
            [datascript.core :as ds]
            [dat.sync.db :as d]))

(defn create-conn []
  (let [conn (ds/create-conn sim/ds-schema)]
                          (p/posh! conn)
                          (d/transact! conn sim/base-ui)
                          conn))

(defn show-ui [conn]
  [sim/sim-selector conn])

(defrecord UIComponent [conn]
  component/Lifecycle
  (start
    [component]
    (let [conn (or conn (create-conn))]
      (reagent/render [show-ui conn] (js/document.getElementById "app"))
      (assoc component :conn conn)))
  (stop [component]
    (assoc component :conn nil)))

(defn new-ui-component []
  (map->UIComponent {}))
