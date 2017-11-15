(ns onyx.sim.components.db
  (:require [com.stuartsierra.component :as component]
            [taoensso.timbre :as log]
            [onyx.sim.core :as sim]
            [posh.reagent :as posh]
            [onyx.sim.event :as event]
            [onyx.sim.dat-view :as dat.view]
            [datascript.core :as d]))

(defn create-conn []
  (let [conn (d/create-conn sim/ds-schema)]
    (posh/posh! conn)
    (d/transact! conn sim/base-ui)
    (event/sim! conn)
    (d/transact! conn sim/examples)
    (d/transact! conn [(dat.view/simulator
                         {:onyx.sim/sim [:onyx/name :dat.view/sim]
                          :dat.sync.db/conn conn})
                       [:db/add [:onyx/name :onyx.sim/settings] :onyx.sim/selected-sim [:onyx/name :dat.view/sim]]])
    (d/transact! conn (dat.view/example))
    conn))

(defrecord KnowledgeBase [conn]
  component/Lifecycle
  (start
    [component]
    (let [conn (or conn (create-conn))]
      (assoc component
        :conn conn)))
  (stop [component]
    (event/unsim! conn)
    (assoc component
      :conn nil)))

(defn new-knowledge-base []
  (map->KnowledgeBase {}))
