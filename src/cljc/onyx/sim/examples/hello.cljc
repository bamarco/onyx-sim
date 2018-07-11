(ns onyx.sim.examples.hello)

(def input-segments
  [{:hello-msg "Hello, World!"}])

(def ^:export my-identity identity)

(def onyx-batch-size 20)

(def job
  {:onyx/type :onyx.core/job
   :onyx/doc "Simulation Example."
   :onyx.sim/label "Hello"
   :onyx.core/catalog [{:onyx/name :in
                        :onyx/type :input
                        :onyx/batch-size onyx-batch-size}
                       {:onyx/name :out
                        :onyx/type :output
                        :onyx/batch-size onyx-batch-size}
                       {:onyx/name :hello
                        :onyx/type :function
                        :onyx/fn ::my-identity
                        :onyx/batch-size onyx-batch-size}]
   :onyx.core/workflow [[:in :hello] [:hello :out]]})
   ;;                      :onyx.core/flow-conditions []
   
