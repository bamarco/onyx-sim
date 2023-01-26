(ns onyx.sim.config
  (:require [environ.core :refer [env]]
            [ring.middleware.defaults :refer [wrap-defaults api-defaults]]
            [ring.middleware.gzip :refer [wrap-gzip]]
            [ring.logger.timbre :refer [wrap-with-logger]]
            ))

(defn config []
  ;; ???: use environ for log file.
  ;; ???: move log config to our own wrap-with-logger middleware that allows for a log file configuration like figwheel does.
;;   (log/merge-config!
;;     {:appenders {:spit (appenders/spit-appender {:fname "log/server.log"})}})
  {:http-port  (Integer. (or (env :port) 10555))
   :middleware [[wrap-defaults api-defaults]
                wrap-with-logger
                wrap-gzip]})
