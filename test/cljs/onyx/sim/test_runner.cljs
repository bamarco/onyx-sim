(ns onyx.sim.test-runner
  (:require
   [doo.runner :refer-macros [doo-tests]]
   [onyx.sim.core-test]
   [onyx.sim.common-test]))

(enable-console-print!)

(doo-tests 'onyx.sim.core-test
           'onyx.sim.common-test)
