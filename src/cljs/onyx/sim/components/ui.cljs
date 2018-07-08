(ns onyx.sim.components.ui
  (:require 
    [com.stuartsierra.component :as component]
    [taoensso.timbre :as log]
    [reagent.core :as reagent]
    [posh.reagent :as posh]
    [onyx.sim.console.ui :as view]
    [onyx.sim.kb :as kb]
    [onyx.sim.components.dispatcher :refer [dispatch!]]
    [datascript.core :as d]
    [onyx.sim.components.sim :as sim]
    [clojure.core.async :as async :refer [go-loop >! <! alts!]]
    [onyx.sim.utils :refer [mapply]]))

(defn show-ui [model]
  [view/selector model])

(defn kill-ui []
  [:div [:p "User Interface has been shut down."]])

(defrecord UI [knowbase dispatcher]
  component/Lifecycle
  (start
    [component]
    (dispatch! (:dispatcher component) :onyx.sim.console.event/init-ui :base-ui view/base-ui)
    (dispatch! (:dispatcher component) :onyx.sim.console.event/init-examples)
    (reagent/render [show-ui component] (js/document.getElementById "app"))
    component)
  (stop [component]
    (reagent/render [kill-ui] (js/document.getElementById "app"))
    component))

(defn new-ui []
  (map->UI {}))
