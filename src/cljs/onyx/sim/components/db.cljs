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
;;     (d/transact! conn [(dat.view/simulator
;;                          {:onyx.sim/sim [:onyx/name :dat.view/sim]
;;                           :dat.sync.db/conn conn})
;;                        [:db/add [:onyx/name :onyx.sim/settings] :onyx.sim/selected-env [:onyx/name :dat.view/sim]]])
    conn))

(defn create-conn2 []
  (let [conn (ds/create-conn sim/ds-schema)
        tx-meta {:datascript.db/tx-middleware onyx.sim.api/middleware}]
    (p/posh! conn)
    (d/transact! conn sim/base-ui2 tx-meta)
;;     (d/transact! conn (dat.view/example))
;;     (d/transact! conn [(dat.view/simulator
;;                          {:onyx.sim/sim [:onyx/name :dat.view/sim]
;;                           :dat.sync.db/conn conn})])
;;     (d/transact!
;;       conn
;;       [[:onyx.sim.api/transition
;;         {:sim [:onyx/name :dat.view/sim]
;;          :event :onyx.sim.api/init
;;          :conn conn}]
;;        [:onyx.sim.api/transition
;;         {:sim [:onyx/name :dat.view/sim]
;;          :event :onyx.sim.api/inputs
;;          :inputs {:dat.view/render [{:dat.view/route :dat.view.route/todos}]}}]]
;;       tx-meta)
    conn))

(defrecord KnowledgeBase [conn]
  component/Lifecycle
  (start
    [component]
    (let [conn (or conn (create-conn2))]
      (assoc component
        :conn conn)))
  (stop [component]
    (assoc component
      :conn nil)))

(defn new-knowledge-base []
  (map->KnowledgeBase {}))
