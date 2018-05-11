(ns onyx.sim.components.ui
  (:require [com.stuartsierra.component :as component]
            [taoensso.timbre :as log]
            [reagent.core :as reagent]
            [posh.reagent :as posh]
            [onyx.sim.ui :as ui]
            [datascript.core :as d]
            [onyx.sim.components.simulator :as sim]
            [onyx.sim.examples.hello :as hello]
            [onyx.sim.examples.flow-short-circuit :as flow]
            [onyx.sim.dat-view :as dat.view]))
            ; [onyx.sim.core :as sim]))

(defn show-ui [sim]
  [ui/selector sim])

(defrecord UIComponent [simulator]
  component/Lifecycle
  (start
    [component]
    (posh/posh! (:conn simulator))
    (d/transact! (:conn simulator) ui/base-ui)
    (sim/submit-job simulator hello/job)
    (sim/submit-job simulator flow/job)
    (reagent/render [show-ui simulator] (js/document.getElementById "app"))
    component)
  (stop [component]
    component))

(defn new-ui-component []
  (map->UIComponent {}))
