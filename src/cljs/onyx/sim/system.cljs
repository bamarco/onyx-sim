(ns onyx.sim.system
  (:require [com.stuartsierra.component :as component]
            [onyx.sim.components.ui        :refer [new-ui-component]]
            [onyx.sim.components.db        :refer [new-knowledge-base]]
            [onyx.sim.components.simulator :refer [new-onyx-sim]]))
;;             [nightlight.repl-server]
            

(declare ^:export system)

(defn new-system []
  (component/system-map
    :knowbase  (new-knowledge-base)
    :simulator (component/using (new-onyx-sim) [:knowbase])
    :app-root  (component/using (new-ui-component) [:knowbase :simulator])))

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
