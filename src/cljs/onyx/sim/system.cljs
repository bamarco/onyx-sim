(ns onyx.sim.system
  (:require [com.stuartsierra.component :as component]
            [onyx.sim.components.ui         :refer [new-ui]]
            [onyx.sim.components.kb         :refer [new-knowledge-base]]
            [onyx.sim.components.datascript :refer [new-datascript]]
            [onyx.sim.components.dispatcher :refer [new-dispatcher]]
            [onyx.sim.components.sim        :refer [new-sim]]))

(declare ^:export system)

(defn new-system []
  (component/system-map
    :datascript (new-datascript)
    :sim (new-sim)
    :knowbase (component/using (new-knowledge-base) [:datascript :sim])
    :dispatcher (component/using (new-dispatcher) [:knowbase])
    :ui  (component/using (new-ui) [:dispatcher :knowbase])))

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
