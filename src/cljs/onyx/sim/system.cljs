(ns onyx.sim.system
  (:require [com.stuartsierra.component :as component]
            [onyx.sim.components.ui        :refer [new-ui-component]]
            [onyx.sim.components.kb        :refer [new-knowledge-base]]
            [onyx.sim.components.datascript :refer [new-datascript]]
            [onyx.sim.components.simulator :refer [new-simulator]]
            [onyx.sim.components.ratom :refer [new-ratom]]))

(declare ^:export system)

(defn new-system []
  (component/system-map
    :datascript (new-datascript)
    :ratom (new-ratom)
    :knowbase (component/using (new-knowledge-base) {:db :datascript
                                                     :state :ratom})
    :simulator (component/using (new-simulator) [:knowbase])
    :ui  (component/using (new-ui-component) [:simulator :knowbase])))

(defn init []
  (set! system (new-system)))

(defn start []
  (set! system (component/start system)))

(defn stop []
  (set! system (component/stop system)))

(defn ^:export go []
  (init)
  (start))

(defn reset []
  (stop)
  (go))
