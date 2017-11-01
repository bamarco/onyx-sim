(ns onyx.sim.components.ui
  (:require [com.stuartsierra.component :as component]
            [reagent.core :as reagent]
            [posh.reagent :as p]
            [onyx.sim.core :as sim]
            [onyx.sim.api :as onyx]
            [datascript.core :as ds]
            [dat.sync.db :as d]))

(defn create-conn []
  (let [conn (ds/create-conn sim/ds-schema)]
    (p/posh! conn)
    (d/transact! conn sim/base-ui)
    conn))

(defn create-dispatcher [conn]
  (fn [{:as event :keys [dat.view/handler]} & inputs]
    (d/transact!
      conn
      [[:db.fn/call
        (onyx/kw->fn handler)
        (assoc
          event
          :dat.view/inputs inputs)]])))

(defn show-ui [conn]
  [sim/sim-selector conn])

(defrecord UIComponent [conn dispatch!]
  component/Lifecycle
  (start
    [component]
    (let [conn (or conn (create-conn))
          dispatch! (or dispatch! (create-dispatcher conn))]
      (reagent/render [show-ui conn] (js/document.getElementById "app"))
      (assoc component
        :conn conn
        :dispatch! dispatch!)))
  (stop [component]
    (assoc component
      :conn nil
      :dispatch! nil)))

(defn new-ui-component []
  (map->UIComponent {}))
