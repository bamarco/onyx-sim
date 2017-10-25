(ns onyx.sim.examples.flow-short-circuit
  (:require [taoensso.timbre :as log]
            [onyx.sim.api :as onyx]))

(def onyx-batch-size 20)

(defn ^:export greater-than-5? [event old {:keys [n]} all-new]
  (> n 5))

(defn ^:export odd-segment? [event old {:keys [n]} all-new]
  (odd? n))

(defn ^:export positive-segment? [event old {:keys [n]} all-new]
  (pos? n))

(defn ^:export my-identity [{:keys [n] :as segment}]
  segment)

(def input-segments
  [{:n 0}
   {:n 1}
   {:n 2}
   {:n 3}
   {:n 4}
   {:n 5}
   {:n 6}
   {:n 7}
   {:n 8}
   {:n 9}])

(def workflow
  [[:in :identity]
   [:identity :out]])

(def catalog
  [{:onyx/name :in
;;     :onyx/plugin :onyx.plugin.core-async/input
    :onyx/type :input
;;     :onyx/medium :core.async
;;     :onyx/max-peers 1
    :onyx/batch-size onyx-batch-size
;;     :onyx/doc "Reads segments from a core.async channel"
    }

   {:onyx/name :identity
    :onyx/fn ::my-identity
    :onyx/type :function
    :onyx/batch-size onyx-batch-size
;;     :parameterized.core/k 42
    }

   {:onyx/name :out
;;     :onyx/plugin :onyx.plugin.core-async/output
    :onyx/type :output
;;     :onyx/medium :core.async
;;     :onyx/max-peers 1
    :onyx/batch-size onyx-batch-size
;;     :onyx/doc "Writes segments to a core.async channel"
    }])

(def flow-conditions
  [{:flow/from :identity
    :flow/to :all
    :flow/short-circuit? true
    :flow/predicate ::greater-than-5?}
   {:flow/from :identity
    :flow/to :none
    :flow/short-circuit? true
    :flow/predicate ::odd-segment?}
   {:flow/from :identity
    :flow/to [:out]
    :flow/short-circuit? false
    :flow/predicate ::positive-segment?}])

;; (def lifecycles
;;   [{:lifecycle/task :in
;;     :lifecycle/calls ::in-calls}
;;    {:lifecycle/task :in
;;     :lifecycle/calls :onyx.plugin.core-async/reader-calls}
;;    {:lifecycle/task :out
;;     :lifecycle/calls ::out-calls}
;;    {:lifecycle/task :out
;;     :lifecycle/calls :onyx.plugin.core-async/writer-calls}])

(def job
  {:onyx.core/catalog catalog
   :onyx.core/workflow workflow
   :onyx.core/flow-conditions flow-conditions
   :onyx.sim/title "Flow Short Circuit" ;; ???: how to break up sim vs job?
   :onyx/doc "An example of flow conditions with short circuit enabled."
;;    :onyx.core/lifecycles lifecycles
   })

