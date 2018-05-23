(ns onyx.sim.test-system
  (:require
    [com.stuartsierra.component :as component]
    [onyx.sim.components.ratom :refer [new-ratom]]
    [onyx.sim.components.datascript :refer [new-datascript]]
    [onyx.sim.components.kb :refer [new-knowledge-base]]
    [onyx.sim.components.simulator :refer [new-simulator]]))

(defn create-system []
  (component/system-map
    :datascript (new-datascript)
    :ratom (new-ratom)
    :knowbase (component/using (new-knowledge-base) {:db :datascript
                                                     :state :ratom})
    :simulator (component/using (new-simulator) [:knowbase])))
