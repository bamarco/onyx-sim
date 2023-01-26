(ns onyx.sim.routes
  (:require [taoensso.timbre :as log]
            [clojure.java.io :as io]
            [compojure.core :refer [ANY GET PUT POST DELETE routes]]
            [compojure.route :refer [resources]]
            [ring.util.response :refer [content-type resource-response response]]))

(defn home-routes [endpoint]
  (routes
   (GET "/" _
        (->
         "onyx/sim/index.html"
         (resource-response  {:root "public"})
         (content-type "text/html")))
    (GET "/favicon.ico" _
         (->
          "onyx/onyx-logo.ico"
          (resource-response {:root "public"})
          (content-type "image/x-icon")))
   (resources "/")))
