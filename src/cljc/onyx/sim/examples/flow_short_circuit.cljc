(ns onyx.sim.examples.flow-short-circuit
  (:require
    [clojure.core.async :as async] ;:refer [close!]]
    [onyx.plugin.core-async]
    [onyx.plugin.seq]
    [onyx.sim.utils :refer [gen-uuid]]))

(def workflow
  [[:in :identity]
   [:identity :out]])

(defn my-identity [{:keys [n] :as segment}]
  segment)

(def capacity 1000)

(def input-chan (async/chan capacity))
(def input-buffer (atom {}))

(def output-chan (async/chan capacity))

(def batch-size 10)

(def catalog
  [{:onyx/name :in
    ; :onyx/plugin :onyx.plugin.core-async/input
    :onyx/plugin :onyx.plugin.seq/input
    :onyx/type :input
    ; :onyx/medium :core.async
    :onyx/max-peers 1
    :onyx/batch-size batch-size
    :onyx/doc "Reads segments from a core.async channel"}

   {:onyx/name :identity
    :onyx/fn ::my-identity
    :onyx/type :function
    :onyx/batch-size batch-size
    :parameterized.core/k 42}

   {:onyx/name :out
    :onyx/plugin :onyx.plugin.core-async/output
    :onyx/type :output
    :onyx/medium :core.async
    :onyx/max-peers 1
    :onyx/batch-size batch-size
    :onyx/doc "Writes segments to a core.async channel"}])

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

(defn greater-than-5? [event old {:keys [n]} all-new]
  (> n 5))

(defn odd-segment? [event old {:keys [n]} all-new]
  (odd? n))

(defn positive-segment? [event old {:keys [n]} all-new]
  (pos? n))

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

; (doseq [segment input-segments]
;   (>!! input-chan segment))

; (close! input-chan)

(def id (gen-uuid))

(def env-config
  {:zookeeper/address "127.0.0.1:2188"
   :zookeeper/server? true
   :zookeeper.server/port 2188
   :onyx/tenancy-id id})

(def peer-config
  {:zookeeper/address "127.0.0.1:2188"
   :onyx/tenancy-id id
   :onyx.peer/job-scheduler :onyx.job-scheduler/balanced
   :onyx.messaging/impl :aeron
   :onyx.messaging/peer-port 40200
   :onyx.messaging/bind-addr "localhost"})

(defn inject-in-ch [event lifecycle]
  {:core.async/buffer input-buffer
   :core.async/chan input-chan})

(defn inject-out-ch [event lifecycle]
  {:core.async/chan output-chan})

(def in-calls
  {:lifecycle/before-task-start inject-in-ch})

(def out-calls
  {:lifecycle/before-task-start inject-out-ch})

(def lifecycles
  [{:lifecycle/task :in
    :seq/sequential input-segments
    :lifecycle/calls :onyx.plugin.seq/inject-seq-via-lifecycle}
    ; :lifecycle/calls ::in-calls}
   {:lifecycle/task :in
    :lifecycle/calls :onyx.plugin.core-async/reader-calls}
   {:lifecycle/task :out
    :lifecycle/calls ::out-calls}
   {:lifecycle/task :out
    :lifecycle/calls :onyx.plugin.core-async/writer-calls}])

(def onyx-batch-size 20)

(def job
  {:onyx/type :onyx.core/job
   :onyx/doc "Flow short circuit example."
   :onyx.sim/label "Flow-Short-Circuit"
   :onyx.core/catalog catalog
   :onyx.core/workflow workflow
   :onyx.core/lifecycles lifecycles
   :onyx.core/flow-conditions flow-conditions})
