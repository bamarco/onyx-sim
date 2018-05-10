; (ns onyx.sim.components.db
;   (:require [com.stuartsierra.component :as component]
;             [taoensso.timbre :as log]
;             ; [onyx.sim.core :as sim]
;             [posh.reagent :as posh]
;             [onyx.sim.dat-view :as dat.view]
;             [onyx.sim.api :as api]
;             [onyx.sim.examples.hello :as hello]
;             [onyx.sim.components.simulator :as sim]
;             [onyx.sim.ui :as ui]
;             [datascript.core :as d]))

; (defn create-conn []
;   (let [conn (d/create-conn (api/idents->schema (into api/schema-idents ui/schema-idents)))];(d/create-conn (merge sim/ds-schema {:onyx.sim.api/job-id {:db/unique :db.unique/identity}}))]
;     (posh/posh! conn)
;     (d/transact! conn ui/base-ui)
;     ; (d/transact! conn [(assoc hello/job :onyx.sim.api/job-id (sim/gen-uuid))])
;     ; (d/transact! conn sim/base-ui)
;     ; (sim/sim! conn)

;     ; (d/transact! conn sim/examples)

;     ; (d/transact! conn [(dat.view/make-sim conn)
;     ;                    [:db/add [:onyx/name :onyx.sim/settings] :onyx.sim/selected-sim [:onyx/name :dat.view/sim]]])
;     ; (d/transact! conn (dat.view/example))

;     conn))

; (defrecord KnowledgeBase [simulator]
;   component/Lifecycle
;   (start
;     [component]
;     (let [conn (or conn (create-conn))]
;       (posh/posh! (:conn simulator))
;       (d/transact! conn ui/base-ui)
;       (assoc component
;         :conn conn)))
;   (stop [component]
;     ; (sim/unsim! conn)
;     (assoc component
;       :conn nil)))

; (defn new-knowledge-base []
;   (map->KnowledgeBase {}))
