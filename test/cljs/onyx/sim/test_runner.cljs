(ns onyx.sim.test-runner
  (:require
   [doo.runner :refer-macros [doo-tests]]
   [onyx.sim.core-test]
   [onyx.sim.components.sim-test]
   [onyx.sim.api-test]))

(enable-console-print!)

(doo-tests 
  'onyx.sim.core-test
  'onyx.sim.components.sim-test
  'onyx.sim.api-test)
