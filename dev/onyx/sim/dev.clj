(ns onyx.sim.dev
  (:require [onyx.sim.application :refer [start system]]))

(def ring-handler
  (let [sys (-> (system)
                (dissoc :http)
                start)]
    (-> sys :handler :handler)))
