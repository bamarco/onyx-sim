(ns onyx.sim.system
  (:require [com.stuartsierra.component :as component]
            [onyx.sim.components.ui :refer [new-ui-component]]
            [nightlight.repl-server]))

(declare system)

(defn new-system []
  (component/system-map
   :app-root (new-ui-component)))

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
