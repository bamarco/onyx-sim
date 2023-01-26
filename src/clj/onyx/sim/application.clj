(ns onyx.sim.application
  (:gen-class)
  (:require [com.stuartsierra.component :as component]
            [system.components.endpoint :refer [new-endpoint]]
            [system.components.handler :refer [new-handler]]
            [system.components.middleware :refer [new-middleware]]
            ;[system.components.jetty :as jetty]
            [system.components.http-kit :as http-kit]
            [onyx.sim.config :refer [config]]
            [onyx.sim.routes :refer [home-routes]]
            [taoensso.timbre :as log]))

(defn new-server [{:as config :keys [http-port]}]
  (log/info "Starting http server on port:" http-port)
  ;; Deprecated way still works:
  (http-kit/new-http-kit :port http-port)
  ;; New way doesn't work yet:
  ;(jetty/new-jetty :port http-port)
  )

(defn app-system
  ([] (app-system (config)))
  ([config]
   (component/system-map
    :routes     (new-endpoint home-routes)
    :middleware (new-middleware {:middleware (:middleware config)})
    :handler    (-> (new-handler)
                    (component/using [:routes :middleware]))
    :http       (-> (new-server config)
                    (component/using [:handler])))))

(defn system []
  (app-system (config)))

(def start component/start)

(defn -main [& _]
  (start (system)))
