(ns onyx.sim.dev
  (:require [onyx.sim.application :refer [run]]))

(def ring-handler
  (let [sys (run)]
    (-> sys :handler :handler)
    ))
