(ns onyx.sim.api-test
  (:require 
    [onyx.sim.api :as onyx]
    [onyx.plugin.seq]
    [onyx.plugin.null]
    [onyx.plugin.core-async]
    [onyx.static.util :refer [kw->fn]]
    [onyx.plugin.protocols :as p]
    [datascript.core :as d]
    [onyx.sim.lifecycle :as lc]
    [clojure.core.async :as async :refer [go go-loop <! >! #?@(:clj [<!! >!!])]]
    [clojure.test :as test :refer [is deftest are testing]]))

(defn test-async
  "Asynchronous test awaiting ch to produce a value or close. https://stackoverflow.com/questions/30766215/how-do-i-unit-test-clojure-core-async-go-macros"
  [ch]
  #?(:clj
      (<!! ch)
      :cljs
      (test/async done
        (async/take! ch (fn [_] (done))))))

(defn test-within
  "Asserts that ch does not close or produce a value within ms. Returns a
  channel from which the value can be taken.
  https://stackoverflow.com/questions/30766215/how-do-i-unit-test-clojure-core-async-go-macros"
  [ms ch]
  (go (let [t (async/timeout ms)
            [v ch] (async/alts! [ch t])]
        (is (not= ch t)
            (str "Test should have finished within " ms "ms."))
        v)))

(defn ^:export seg-identity [seg]
  seg)

(defn ^:export seg-identity-async [seg]
  (let [async-seg (async/promise-chan)]
    (go (>! async-seg [seg]))
    async-seg))

(def batch-size 20)

(def identity-job
  {:catalog [{:onyx/name       :in
              :onyx/type       :input
              :onyx/batch-size batch-size}
             {:onyx/name       ::identity
              :onyx/fn         ::seg-identity
              :onyx/type       :function
              :onyx/batch-size batch-size}
             {:onyx/name       :out
              :onyx/type       :output
              :onyx/batch-size batch-size}]
   :workflow [[:in ::identity] [::identity :out]]})

(def identity-async-job
  {:catalog [{:onyx/name       :in
              :onyx/type       :input
              :onyx/batch-size batch-size}
             {:onyx/name       ::identity
              :onyx/fn         ::seg-identity-async
              :onyx/type       :function
              :onyx/batch-size batch-size}
             {:onyx/name       :out
              :onyx/type       :output
              :onyx/batch-size batch-size}]
    :workflow [[:in ::identity] [::identity :out]]})

(defn seq->out-job [& {:as opts :keys [batch-size] :or {batch-size 20}}]
  {:catalog 
   [{:onyx/type :input
     :onyx/plugin :onyx.plugin.seq
     :onyx/batch-size batch-size
     :onyx/name :in}
    {:onyx/name       ::identity
     :onyx/fn         ::seg-identity-async
     :onyx/type       :function
     :onyx/batch-size batch-size}
    {:onyx/name       :out
     :onyx/type       :output
     :onyx/batch-size batch-size}]

   :workflow 
   [[:in ::identity] 
    [::identity :out]]

   :lifecycles
   [{:lifecycle/task :in
     :seq/sequential [{:hello 1} {:hello 2} {:hello 3}]
     :lifecycle/calls :onyx.plugin.seq/inject-seq-via-lifecycle}]})
  
(defn seq->chan-job [chan-id]
  {:catalog 
   [{:onyx/type :input
     :onyx/plugin :onyx.plugin.seq
     :onyx/batch-size batch-size
     :onyx/name :in}
    {:onyx/name       ::identity
     :onyx/fn         ::seg-identity-async
     :onyx/type       :function
     :onyx/batch-size batch-size}
    {:onyx/type :output
     :onyx/batch-size batch-size
     :onyx/name :out
     :onyx/plugin :onyx.plugin.core-async
     :onyx/medium :core.async}]

   :workflow 
   [[:in ::identity] 
    [::identity :out]]

   :lifecycles
   [{:lifecycle/task :in
     :seq/sequential [{:hello 1} {:hello 2} {:hello 3}]
     :lifecycle/calls :onyx.plugin.seq/inject-seq-via-lifecycle}
    {:lifecycle/task :out
     :lifecycle/calls :onyx.plugin.core-async/out-calls
     :core.async/id chan-id}]})

(def chan->chan-job
  {:catalog
   [{:onyx/name       :in
      :onyx/type       :input
      :onyx/plugin     :onyx.plugin.core-async
      :onyx/medium     :core.async
      :onyx/batch-size batch-size}
    {:onyx/name       :out
      :onyx/type       :output
      :onyx/plugin     :onyx.plugin.core-async
      :onyx/medium     :core.async
      :onyx/batch-size batch-size}]

   :workflow
   [[:in :out]]

   :lifecycles
   [{:lifecycle/task :in
      :lifecycle/calls :onyx.sim.lifecycle/resource-calls
      :onyx.sim.lifecycle/inject {:core.async/chan [:in>]
                                  :core.async/buffer  [:in-buf]}}
    {:lifecycle/task :out
      :lifecycle/calls :onyx.sim.lifecycle/resource-calls
      :onyx.sim.lifecycle/inject {:core.async/chan [:out>]}}]})

(deftest test-new-inputs
  (let [env (-> (onyx/init identity-job)
                (onyx/new-inputs {:in [{:hello 1} {:hello 2} {:hello 3}]}))]
    (is (get-in env [:tasks :in :inbox]) [{:hello 1} {:hello 2} {:hello 3}])))

(deftest test-remove-outputs
  (let [env (-> (onyx/init identity-job)
                (onyx/new-inputs {:in [{:hello 1} {:hello 2} {:hello 3}]})
                (onyx/drain)
                (onyx/remove-outputs {:out [{:hello 1}]}))]
    (is (get-in env [:tasks :out :outputs]) [{:hello 2} {:hello 3}])))

(deftest test-drain
  (let [env (-> identity-job
              (onyx/init)
              (onyx/new-inputs {:in [{:hello 1}]})
              (onyx/drain))]
    (is
      (=
        (get-in env [:tasks :out :outputs])
        [{:hello 1}]))))

(deftest test-go-drain
  (let [env-chan (-> identity-job
                    (onyx/init)
                    (onyx/new-inputs {:in [{:hello 1} {:hello 2} {:hello 3}]})
                    (onyx/go-drain))
        async-env-chan (-> identity-async-job
                          (onyx/init)
                          (onyx/new-inputs {:in [{:hello 1} {:hello 2} {:hello 3}]})
                          (onyx/go-drain))]
    (test-async
      (test-within 1000
        (go 
          (let [env (<! env-chan)]
            (is 
              (=
                (get-in env [:tasks :out :outputs])
                [{:hello 1} {:hello 2} {:hello 3}]))))))
    (test-async
      (test-within 1000
        (go 
          (let [env (<! async-env-chan)]
            (is
              (=
                (get-in env [:tasks :out :outputs])
                [{:hello 1} {:hello 2} {:hello 3}]))))))))

(deftest test-go-tick
  (let [env-chan (-> (onyx/init identity-async-job)
                     (onyx/new-inputs {:in [{:hello 1} {:hello 2} {:hello 3}]})
                     (onyx/go-tick 500))]
    (test-async
      (test-within 1000
        (go
          (let [env (<! env-chan)]
            (is
              (=
                (get-in env [:tasks :out :outputs])
                [{:hello 1} {:hello 2} {:hello 3}]))))))))

(deftest test-poll-job
  (let [env (-> (seq->out-job :batch-size 20)
              (onyx/init)
              (onyx/transition-env :onyx.sim.api/poll!))
        _ (is (= 
                (get-in env [:tasks :in :inbox]) 
                [{:hello 1} {:hello 2} {:hello 3}]))
        batch-env (-> (seq->out-job :batch-size 2)
                      (onyx/init)
                      (onyx/transition-env :onyx.sim.api/poll!))]
    (test-async
      (test-within 1000
        (go
          (let [env (onyx/<transition-env env :onyx.sim.api/go-drain)]
            (is
              (= 
                (get-in env [:tasks :out :outputs])
                [{:hello 1} {:hello 2} {:hello 3}]))))))
    (test-async
      (test-within 1000
        (go
          (let [env (onyx/<transition-env batch-env :onyx.sim.api/go-drain)]
            (is
              (=
                (get-in env [:tasks :out :outputs])
                [{:hello 1} {:hello 2}]))))))))

(deftest test-chan-plugin
  (let [env (-> (seq->chan-job 42)
                (onyx/init)
                (onyx/transition-env :onyx.sim.api/poll!))]
    (test-async
      (test-within 1000
        (go
          (let [drained-env (onyx/<transition-env env :onyx.sim.api/go-drain)
                flushed-env (onyx/<transition-env drained-env :onyx.sim.api/go-write!)]
            (is
              (=
                (get-in drained-env [:tasks :out :outputs])
                [{:hello 1} {:hello 2} {:hello 3}]))
            (is
              (=
                (get-in flushed-env [:tasks :out :outputs])
                []))))))
    (test-async
      (test-within 1000
        (go
          (let [chan (onyx.plugin.core-async/get-channel 42)
                out [(<! chan) (<! chan) (<! chan)]]
            (is
              (=
                out
                [{:hello 1} {:hello 2} {:hello 3}]))))))))

(deftest test-go-job
  (let [env-chan (onyx/go-job! (seq->out-job))]
    (test-async
      (test-within 1000
        (go
          (let [env (<! env-chan)]
            (is
              (=
                (get-in env [:tasks :out :outputs])
                [{:hello 1} {:hello 2} {:hello 3}]))))))))

(deftest test-full-async-job
  (let [in> (async/chan)
        out> (async/chan)
        job (lc/bind-resources chan->chan-job {:in>  in>
                                               :in-buf (atom {})
                                               :out> out>})
        _ (onyx/go-job! job)
        _ (go (async/onto-chan in> [{:hello 1} {:hello 2} {:hello 3}]))]
    (test-async
      (test-within 1000
        (go
          (let [outs [(<! out>) (<! out>) (<! out>)]]
            (is 
              (= outs 
                [{:hello 1} {:hello 2} {:hello 3}]))))))))


(defn create-sim []
  {:envs (atom {})
   :control> (async/chan)
   :dat.sync.db/transact! d/transact!
   :dat.sync.db/q d/q
   :dat.sync.db/pull d/pull
   :dat.sync.db/deref deref
   :dat.sync.db/conn (d/create-conn {:onyx.sim.api/job-id {:db/unique :db.unique/identity}})})
   

(deftest test-submit-job
  (let [in> (async/chan)
        out> (async/chan)
        job (lc/bind-resources chan->chan-job {:in>  in>
                                                :in-buf (atom {})
                                                :out> out>})
        in2> (async/chan)
        out2> (async/chan)
        job2 (lc/bind-resources chan->chan-job {:in>  in2>
                                                :in-buf (atom {})
                                                :out> out2>})
        sim (create-sim)]
    (onyx/submit-job sim job)
    (onyx/submit-job sim job2)
    (onyx/go-schedule-jobs! sim)
    (go (async/onto-chan in> [{:hello 1} {:hello 2} {:hello 3}]))
    (go (async/onto-chan in2> [{:hello 4} {:hello 5} {:hello 6}]))

    (test-async
      (test-within 1000
        (go
          (let [outs [(<! out>) (<! out>) (<! out>)]]
            (is
              (=
                outs
                [{:hello 1} {:hello 2} {:hello 3}]))))))
    (test-async
      (test-within 1000
        (go
          (let [outs [(<! out2>) (<! out2>) (<! out2>)]]
            (is
              (=
                outs
                [{:hello 4} {:hello 5} {:hello 6}]))))))))
