(ns onyx.sim.api
  (:require [taoensso.timbre :as log]
            [onyx-local-rt.api :as onyx]
            #?(:cljs [reagent.ratom :as ratom])
            [onyx.static.util :as util]
            [dat.sync.db :as d]
            [onyx.sim.utils #?@(:clj (:refer [xfn])
                                     :cljs (:refer-macros [xfn]))]))


(defonce onyx-batch-size 20)

(def env-summary     onyx/env-summary)
(def init            onyx/init)
(def tick            onyx/tick)
(def drain           onyx/drain)
(def transition-env  onyx/transition-env)
(def new-segment     onyx/new-segment)
(def kw->fn          util/kw->fn)

;; TODO: Additionally, :metadata may optionally contain a :job-id key. When specified, this key will be used for the job ID instead of a randomly chosen UUID. Repeated submissions of a job with the same :job-id will be treated as an idempotent action. If a job with the same ID has been submitted more than once, the original task IDs associated with the catalog will be returned, and the job will not run again, even if it has been killed or completed. It is undefined behavior to submit two jobs with the same :job-id metadata whose :workflow, :catalog, :flow-conditions, etc are not equal.

(defn ds->onyx [sim-job]
  (-> sim-job
      (clojure.set/rename-keys {:onyx.core/catalog         :catalog
                                :onyx.core/workflow        :workflow
                                :onyx.core/lifecycles      :lifecycles
                                :onyx.core/windows         :windows
                                :onyx.core/triggers        :triggers
                                :onyx.core/task-scheduler  :task-scheduler
                                :onyc.core/metadata        :metadata
                                :onyx.core/flow-conditions :flow-conditions})))

(defn new-inputs [env inputs]
  (reduce
    (fn [env [task-name segments]]
      (reduce
        (fn [env segment]
          (onyx/new-segment env task-name segment))
        env
        segments))
    env
    inputs))

(defmethod onyx/transition-env ::inputs
  [env {:keys [inputs]}]
  (log/info ::inputs)
  (new-inputs env inputs))

(defmethod onyx/transition-env ::init
  [env {:keys [sim db]}]
  (let [job (ds->onyx (:onyx.core/job (d/pull db '[{:onyx.core/job [{:onyx.core/catalog [*]} *]}] sim)))]
    (log/info ::init)
    (onyx/init job)))

(defn meta-transact [{:as report :keys [db-before db-after]} meta-key meta-fn tx]
  (log/info "meta-tx" tx)
  (let [db-after (with-meta
                   db-after
                   (update-in
                     (or (meta db-after) (meta db-before) {})
                     [meta-key]
                     (fn [env]
                       (meta-fn env db-after (rest tx)))))]
    (log/info "db-after")
    (assoc
      report
      :db-after
      db-after)))

(defn meta-tx? [meta-key tx]
  (and
    (vector? tx)
    (= meta-key (first tx))))

(defn meta-transactor
  "This allows you to have plain clojure data that shares a transactor with the db-conn."
  [transact meta-key meta-reducing-fn]
  (fn [report txs]
    (reduce
      (fn [report tx]
        (log/info "tx" tx)
        (if (meta-tx? meta-key tx)
          (meta-transact report meta-key meta-reducing-fn tx)
          (transact report [tx])))
      report
      txs)))

(defn mw-transition
  "Looks for simulator transitions and runs them on the env cache."
  [transact]
  (log/info "onyx.sim.api/middleware")
  (meta-transactor
    transact
    ::transition
    (fn [cache db [{:as transition
                    :keys [sim]}]]
         (let [cache-after (update-in
                             (or cache {})
                             [(:db/id (d/entity db sim))]
                             onyx/transition-env
                             (assoc transition :db db))]
           (log/info ::transition transition)
           cache-after))))

(def middleware (comp mw-transition d/mw-keep-meta))

(defn out
  "Returns outputs of onyx job presumably after draining."
  [env]
  (into
    {}
    (comp
      (filter (fn [[_ task]] (= (get-in task [:event :onyx.core/task-map :onyx/type]) :output)))
      (xfn [[task-name task]]
           [task-name (:outputs task)]))
    (:tasks env)))

(defn run
  "Drains and stops an onyx environment with the given segments fed in."
  [env in]
  (-> (reduce (fn [env [task segment]]
                 (onyx/new-segment env task segment)) env in)
      (onyx/drain)
      (onyx/stop)))

(defn step
  "Ticks until the start of the next task."
  [env]
  (reduce
    (fn [env _]
      (if (= (:next-action env) :lifecycle/after-batch)
        (reduced (onyx/tick env))
        (onyx/tick env)))
    (onyx/tick env)
    (range 1000)))

(defn remove-segment [env output-task segment]
  (onyx/transition-env env {:event :remove-segment
                            :task output-task
                            :segment segment}))

(defmethod onyx/transition-env :remove-segment
  [env {:keys [task segment]}]
  (update-in env [:tasks task :outputs] (partial into [] (remove #(= segment %)))))

(defn onyx-feed-loop [env & selections]
  ;; TODO: :in and :render need to be genericized
  (reduce
    (fn [env selection]
      (-> (onyx/new-segment env :in selection)
          (remove-segment :render selection)))
    env
    selections))



