(ns onyx.sim.common-test
  (:require 
    [onyx.sim.api :as onyx]
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

; (def batch-size 20)

; (defn ^:export hello [{:as seg :keys []}]
;   (assoc seg :hello-msg "Hello, world!"))

; (defn ^:export hello-label [{:as seg :keys [hello-msg]}]
;   (assoc seg ::render [:div [:p hello-msg]]))

; (def job 
;   {:workflow     [[:in  :hello]
;                   [:hello :out]
;                   [:hello :hello-label]
;                   [:hello-label :mount]]
;    :catalog      [{:onyx/type :input
;                    :onyx/name :in
;                    :onyx/batch-size batch-size}
;                   {:onyx/type :input
;                    :onyx/name :route
;                    :route/path "/"}
;                   {:onyx/type :function
;                    :onyx/name :hello
;                    :onyx/fn ::hello
;                    :onyx/batch-size batch-size}
;                   {:onyx/type :output
;                    :onyx/name :out
;                    :onyx/batch-size batch-size}
;                   {:onyx/type :input
;                    :onyx/plugin ::reagent
;                    ::reagent-props `[hello-msg]}
;                   {:onyx/type :function
;                    :onyx/name :dat.view
;                    :dat.view/alias '{:dat.view/label hello-msg}}
;                   {:onyx/type :function
;                    :onyx/name :label
;                    :onyx/fn   ::label}
;                   {:onyx/type :function
;                    :onyx/name :hello-label
;                    :onyx/fn ::hello-label
;                    :onyx/batch-size batch-size}
;                   {:onyx/type :output
;                    :onyx/name :mount
;                    :onyx/plugin ::mount
;                    ::mount-point "app"
;                    :onyx/batch-size batch-size}]
;    :flow-conditions []})

; ; (defn bidi-route [routes {:keys [window/path]}]
; ;   (bidi/route routes path))

; (defn reagent-render [mount-point {:as env}]
;   (r/render
;     mount-point
;     (->
;       env
;       ::mount
;       ::render)))

; (def main-job
;   {:workflow [[:q :box] [:box :mount]]
;    :catalog [{:onyx/type    :input
;               :onyx/plugin  :bidi
;               :bidi/route   {"/" hello-job}}
;              {:onyx/type :function
;               :onyx/name :render-job}
;              {:onyx/type     :output
;               :onyx/plugin   :reagent
;               :reagent/mount "app-hook"}]})

; (def hello-job
;   {:workflow [[:in :alias] [:alias :render]]
;    :catalog [{:onyx/type :input
;               :onyx/plugin :datahike
;               :datahike/pull-expr `[::hello-msg]
;               :datahike/entity    [:db/ident ::hello]}
;              {:onyx/type  :function
;               :onyx/alias {:dat.view/label [::hello-msg]}}
;              {:onyx/type  :function
;               :onyx/name  :dat.view/label}
;              {:onyx/type   :output
;               :onyx/plugin :dat.view/render}]})

; (def job-pull-expr '[{:catalog [*] :workflow [*]} *])

; (defn submit-jobs [{:as job}]
;   (submit-job job 8000))

; (defn env-out [env]
;   (->
;     env
;     onyx/out))

; (defn ensure-job! [conn job]
;   (let [sim-id (or (:onyx.sim/sim job) (hash job))]
;     (d/transact! 
;       conn 
;       [{:onyx.sim/job job
;         :onyx.sim/sim sim-id}])))

; (defn env-for-job [conn job]
;   (let [sim-id (or (:onyx.sim/sim job) (hash job))]
;     (event/subscribe-env conn sim-id)))

; (defn submit-job [conn job]
;   (ensure-job! conn job)
;   (env-for-job conn job))

; (defn render-job-async [conn job]
;   (let [sim-id        (update-in job [:onyx.sim/sim-id] #(if % % (hash job)))
;         _             (submit-job conn job)
;         subscriber-id (event/gen-id) ;; ???: can we just grab the react component id instead?
;         env           (event/subscribe-env conn subscriber-id job)]
;     (r/create-class
;       :component-will-unmount
;       #(event/unsubsribe-env conn subscriber-id job)

;       :display-name 
;       (or (:onyx/name job) (str "Job #" (:onyx.sim/sim-id job)))

;       :reagent-render
;       (fn [job]
;         (let [env @(submit-job conn job)]
;           (->
;             env
;             ::mount
;             ::render))))))

; (defn render-job [job]
;   (->
;     (onyx/init job)
;     onyx/drain
;     onyx/out
;     ::mount
;     ::render))

; (defn dat-view-box [children env-out]
;   {::render
;     (fn [inputs]
;       [re-com/children
;        :children
;        (mapv render-job (::mount env-out))])})

; (def box-job
;   {:workflow [[:in :layout]
;               [:layout :render]]
;    :catalog [{:onyx/type   :input
;               :onyx/plugin :datahike
;               :datahike/q  {:find ('pull '?job job-pull-expr)
;                             :where [['?job :dat.view/main true]]}}
;              {:onyx/type         :function
;               :dat.view/defaults {:dat.view/align :dat.view/vertical}
;               :onyx/name         :dat.view/box}
;              {:onyx/type :output
;               :onyx/name ::mount
;               :onyx/plugin :dat.view/render}]})
              
; (def app-hook
;   {:catalog  
;    [{:onyx/type     :output
;      :onyx/plugin   :reagent
;      :onyx/name     :app-hook
;      :reagent/mount "app-hook"}]})

; (def register-hello
;   [{:dat.view/route "/"
;     :dat.view/job-fragments [hello-job app-hook]
;     :dat.view/job {}}
;    {:dat.view/route ""}])

; (def job-cache (atom {}))

; (defn cache-job [job]
;   (assoc job-cache))

; (defn submit-job-async! [job-chan env-atom]
;   (go-loop []
;     (let [timeout 8000
;           job (<! job-chan)
;           env (onyx/submit-job job timeout)]
;       (reset! env-atom (!< env)))))

; (defn display-env [env-atom]
;   (-> 
;     @env-atom
;     :onyx/out
;     ::mount
;     ::render))

; (defn bind-inputs [env {:as job} {:as inputs}]
;   (let [old (::inputs env)]
;     (when-not (= old inputs)
;       (->
;         (onyx/init job)
;         (onyx/new-inputs inputs)
;         onyx/drain))))

; (defn render-job
;   "Job can never change"
;   [job _]
;   (let [env (atom {})
;         job-chan (async/chan)]
;     (submit-job-async! job-chan env-atom)
;     (fn [job inputs]
;       (:inputs @env)
;       (bind-props env hello-label)
;       (go (>! job-chan job))
;       [display-env env])))

; (defn box [{::keys [layout]}]
;   {::render
;     [re-com/v-box
;      :children
;      (mapv
;        (fn [job]
;          [render-job job])
;        layout)]})

; (defn submit-job2 [conn job]
;   (d/transact!
;     [job
;      [:onyx.sim/transtions [{:event :onyx.sim.api/drain-or-park}]]]))

; (defn go-jobs [conn job-atom]
;   (go-loop []
;     (let [[seg [job-id task-id out-index] (alts! parked-jobs)]]
;       (transact!
;         conn
;         [:onyx.sim/transitions [{:event   :onyx.sim.api/assoc-segment
;                                  :task    task-id
;                                  :segment seg
;                                  :index   seg-index}
;                                 ;; This doesn't translate to remotes very well. At its heart it's a local action
;                                 {:event   :onyx.sim.api/drain-or-park}]]))))
                                

; (deftest test-job
;   (is
;     (-> 
;       (onyx/init job)
;       (onyx/new-inputs {:in [{}]})
;       onyx/drain
;       onyx/out)
;     {:out [{:hello-msg "Hello, world!"}]}))

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
              :onyx/batch-size batch-size}
             {:onyx/name       :out
              :onyx/type       :output
              :onyx/batch-size batch-size}]
    :workflow [[:in ::identity] [::identity :out]]})

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

(deftest test-go-drain
  (let [env-chan (-> (onyx/init identity-job)
                     (onyx/new-inputs {:in [{:hello 1}]})
                     (onyx/go-drain))
        async-env-chan (-> (onyx/init identity-async-job)
                           (onyx/new-inputs {:in [{:hello 1}]})
                           (onyx/go-drain))]
    (test-async
      (test-within 1000
        (go 
          (let [env (<! env-chan)]
            (is (get-in env [:tasks :out :outputs]) [{:hello 1}])))))
    (test-async
      (test-within 1000
        (go 
          (let [env (<! async-env-chan)]
            (is (get-in env [:tasks :out :outputs]) [{:hello 1}])))))))


(deftest test-go-tick
  (let [env-chan (-> (onyx/init identity-async-job)
                     (onyx/new-inputs {:in [{:hello 1}]})
                     (onyx/go-tick 500))]
    (test-async
      (test-within 1000
        (go
          (let [env (<! env-chan)]
            (is (get-in env [:tasks :out :outputs]) [{:hello 1}])))))))

; (deftest test-async-job
;   (let [chan (onyx/submit-job-naive job 8000)]
;     (go
;       (is 
;         (onyx/out (<! chan))
;         {:out []}))))
