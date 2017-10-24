(ns onyx.sim.api
  (:require [onyx-local-rt.api :as onyx]
            [onyx.static.util :as util]
            [onyx.sim.utils #?@(:clj (:refer [xfn])
                                     :cljs (:refer-macros [xfn]))]))


(defonce onyx-batch-size 20)

(def env-summary onyx/env-summary)
(def init        onyx/init)
(def tick        onyx/tick)
(def drain       onyx/drain)
(def new-segment onyx/new-segment)
(def kw->fn      util/kw->fn)

;; TODO: Additionally, :metadata may optionally contain a :job-id key. When specified, this key will be used for the job ID instead of a randomly chosen UUID. Repeated submissions of a job with the same :job-id will be treated as an idempotent action. If a job with the same ID has been submitted more than once, the original task IDs associated with the catalog will be returned, and the job will not run again, even if it has been killed or completed. It is undefined behavior to submit two jobs with the same :job-id metadata whose :workflow, :catalog, :flow-conditions, etc are not equal.

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



