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

(defn create-conn []
  (let [conn (ds/create-conn sim/ds-schema)]
    (p/posh! conn)
    (d/transact! conn sim/base-ui)
    (d/transact! conn (dat.view/example))
    (d/transact! conn [(dat.view/simulator)
                       [:db/add [:onyx/name :onyx.sim/settings] :onyx.sim/selected-env [:onyx/name :dat.view/sim]]])
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
