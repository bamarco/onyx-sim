(ns onyx.sim.components.db
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
    (d/transact! conn [(dat.view/simulator {:dat.sync.db/conn conn
                                            :onyx.sim/sim [:onyx/name :dat.view/sim]})
                       [:db/add [:onyx/name :onyx.sim/settings] :onyx.sim/selected-env [:onyx/name :dat.view/sim]]])
    conn))

(defrecord KnowledgeBase [conn]
  component/Lifecycle
  (start
    [component]
    (let [conn (or conn (create-conn))]
      (assoc component
        :conn conn)))
  (stop [component]
    (assoc component
      :conn nil)))

(defn new-knowledge-base []
  (map->KnowledgeBase {}))
