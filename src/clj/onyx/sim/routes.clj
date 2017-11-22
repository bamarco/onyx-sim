(ns onyx.sim.routes
  (:require [taoensso.timbre :as log]
            [clojure.java.io :as io]
            [compojure.core :refer [ANY GET PUT POST DELETE routes]]
            [compojure.route :refer [resources]]
            [hiccup.core :as hiccup]
            [hiccup.form :as form]
            [hiccup.page :as page]
            [ring.util.response :refer [response]]))

(defn home-routes [endpoint]
  (routes
   (GET "/" _
     (-> "public/ns/onyx/sim/index.html"
         io/resource
         io/input-stream
         response
         (assoc :headers {"Content-Type" "text/html; charset=utf-8"})))
    (GET "/favicon.ico" _
     (-> "public/ns/onyx/onyx-logo.ico"
         io/resource
         io/input-stream
         response
         (assoc :headers {"Content-Type" "text/html; charset=utf-8"})))
   (resources "/")))
