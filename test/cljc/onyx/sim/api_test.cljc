(ns onyx.sim.api-test
  (:require 
    [onyx.sim.api :as onyx]
    [onyx.plugin.seq]
    [onyx.static.util :refer [kw->fn]]
    [onyx.plugin.protocols :as p]
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

(def a-seq-plugin
  {:seq/seq [{:hello 1} {:hello 2} {:hello 3}]
   :onyx/type :input
   :onyx/plugin :onyx.plugin.seq
   :onyx/batch-size batch-size
   :onyx/name :in})

(def a-plugin-job
  {:catalog [a-seq-plugin
             {:onyx/name       ::identity
              :onyx/fn         ::seg-identity-async
              :onyx/type       :function
              :onyx/batch-size batch-size}
             {:onyx/name       :out
              :onyx/type       :output
              :onyx/batch-size batch-size}]
   :workflow [[:in ::identity] [::identity :out]]})

(def batched-plugin-job
  {:catalog 
   [{:seq/seq [{:hello 1} {:hello 2} {:hello 3}]
     :onyx/type :input
     :onyx/plugin :onyx.plugin.seq
     :onyx/batch-size 1
     :onyx/name :in}
    {:onyx/name       ::identity
     :onyx/fn         ::seg-identity-async
     :onyx/type       :function
     :onyx/batch-size 1}
    {:onyx/name       :out
     :onyx/type       :output
     :onyx/batch-size 1}]

   :workflow
   [[:in ::identity] [::identity :out]]})

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

(deftest test-poll
  (let [pipeline (onyx.plugin.seq/input a-seq-plugin)
        event (:event pipeline)
        timeout 1000
        _ (p/recover! pipeline nil nil)
        chunk (p/poll! pipeline event timeout)]
    (is 
      chunk
      [{:hello 1}])))

(deftest test-poll-job
  (let [env (-> a-plugin-job
              (onyx/init)
              (onyx/transition-env {:onyx.sim.api/event :onyx.sim.api/poll!}))
        _ (is (= 
                (get-in env [:tasks :in :inbox]) 
                [{:hello 1} {:hello 2} {:hello 3}]))
        env-chan (onyx/go-drain env)
        batch-env-chan (-> batched-plugin-job
                         (onyx/init)
                         (onyx/go-drain))]
    (test-async
      (test-within 1000
        (go
          (let [env (<! env-chan)]
                ; polls (onyx/poll-plugins! env)]
            ; (is 
            ;   (=
            ;     polls
            ;     {:in [{:hello 1} {:hello 2} {:hello 3}]}))
            (is
              (= 
                (get-in env [:tasks :out :outputs])
                [{:hello 1} {:hello 2} {:hello 3}]))))))))
    ; (test-async
    ;   (test-within 1000
    ;     (go
    ;       (let [env (<! batch-env-chan)]
    ;         (is
    ;           (=
    ;             (get-in env [:tasks :out :outputs])
    ;             [{:hello 1} {:bullshit true}]))))))))

