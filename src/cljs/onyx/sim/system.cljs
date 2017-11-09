(ns onyx.sim.system
  (:require [com.stuartsierra.component :as component]
            [onyx.sim.components.ui :refer [new-ui-component]]
            [onyx.sim.components.db :refer [new-knowledge-base]]
;;             [nightlight.repl-server]
            ))

(declare ^:export system)

(declare launch-system)

(defn new-system []
  (component/system-map
    :knowbase (new-knowledge-base)
    :app-root (component/using (new-ui-component) [:knowbase])))

(defn new-resource-system
  "Need a separate resource system so dat view lifecycle can find it. Not really sure what the best way to inject the conn into onyx is since onyx itself is inside the conn."
  []
  (component/system-map
    :knowbase (new-knowledge-base)))

(defn new-launch-system [resource-system]
  (component/system-map
    :knowbase (:knowbase resource-system)
    :app-root (component/using (new-ui-component) [:knowbase])))

(defn init []
  (set! system (new-system)))

(defn init2 []
  (set! system (new-resource-system))
  (set! launch-system (new-launch-system system)))

(defn start []
  (set! system (component/start system)))

(defn start2 []
  (set! system (component/start system))
  (set! launch-system (component/start launch-system)))

(defn stop []
  (set! system (component/stop system)))

(defn stop2 []
  (set! system (component/stop system))
  (set! launch-system (component/stop launch-system)))

(defn ^:export go []
  (init)
  (start))

(defn reset []
  (stop)
  (go))
