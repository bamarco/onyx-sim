(ns onyx.sim.components.sim-test
  (:require
    [clojure.test :as test :refer [is deftest are testing]]
    #?(:clj [clj-time.core :as time]
       :cljs [cljs-time.core :as time])
    [onyx.sim.components.sim :as sim]))

(deftest test-min-time
  (let [now (time/now)
        later (time/plus now (time/seconds 6))]
    (is (= (sim/min-time nil nil) nil))
    (is (time/equal? (sim/min-time nil now) now))
    (is (time/equal? (sim/min-time now nil) now))
    (is (time/equal? (sim/min-time now later) now))
    (is (time/equal? (sim/min-time later now) now))))

(deftest test-fire-transition?
  (let [now (time/now)
        as-often-as-possible (time/seconds 0)]
    (is (sim/fire-transition? now {:onyx.sim.components.sim/last-fired now :onyx.sim.components.sim/frequency as-often-as-possible}))))
    
