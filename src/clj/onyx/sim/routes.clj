(ns onyx.sim.routes
  (:require [taoensso.timbre :as log]
            [compojure.core :refer [ANY GET PUT POST DELETE routes]]
            [compojure.route :refer [resources]]
            [ring.util.response :refer [content-type resource-response redirect response]]))

(defn home-routes [endpoint]
  (log/info "example" (->
         "index.html"
         (resource-response  {:root "public"})
         (content-type "text/html")))
  (routes
   (GET "/" _ (redirect "onyx-sim"))
   (GET "/onyx-sim" _
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
