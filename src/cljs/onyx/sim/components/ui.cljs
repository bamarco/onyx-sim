(ns onyx.sim.components.ui
  (:require [com.stuartsierra.component :as component]
            [taoensso.timbre :as log]
            [reagent.core :as reagent]
            [posh.reagent :as posh]
            [onyx.sim.ui :as ui]
            [onyx.sim.kb :as kb]
            [datascript.core :as d]
            [onyx.sim.components.simulator :as sim]
            [onyx.sim.examples.hello :as hello]
            [onyx.sim.examples.flow-short-circuit :as flow]
            [clojure.core.async :as async :refer [go-loop >! <! alts!]]
            [onyx.sim.utils :refer [mapply]]
            [onyx.sim.dat-view :as dat.view]))
            ; [onyx.sim.core :as sim]))

(defn go-event! [{:as sim :keys [knowbase]} event>]
  (let [control> (async/chan)]
    (go-loop []
      (let [[ch event] (alts! (<! event>) control>)]
        (if (= ch control>)
          (when (not= event ::kill)
            (log/warn "unknown control>" event)
            (recur))
          (do
            (kb/transact! sim #(ui/handler % event))
            (recur)))))))

(defn show-ui [sim]
  [ui/selector sim])

(defrecord UIComponent [simulator knowbase]
  component/Lifecycle
  (start
    [component]
    (kb/transact! knowbase (fn [kbs] {:db ui/base-ui}))
    (sim/submit-job simulator hello/job)
    (sim/submit-job simulator flow/job)
    (reagent/render [show-ui simulator] (js/document.getElementById "app"))
    component)
  (stop [component]
    component))

(defn new-ui-component []
  (map->UIComponent {}))
