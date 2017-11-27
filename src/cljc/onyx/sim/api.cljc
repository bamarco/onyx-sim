(ns onyx.sim.api
  (:require [taoensso.timbre :as log]
            [onyx-local-rt.api :as onyx]
            [onyx.static.util]
            [onyx.sim.utils #?@(:clj (:refer [xfn])
                                     :cljs (:refer-macros [xfn]))]))


(defonce onyx-batch-size 20)

(def env-summary     onyx/env-summary)
(def init            onyx/init)
(def tick            onyx/tick)
(def drain           onyx/drain)
(def transition-env  onyx/transition-env)
(def new-segment     onyx/new-segment)
(def ^:private kw->fn          onyx.static.util/kw->fn)

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
  (onyx/transition-env env {:event ::remove-segment
                            :task output-task
                            :segment segment}))

(defmethod onyx/transition-env ::inputs
  [env {:keys [inputs]}]
  (new-inputs env inputs))

(defmethod onyx/transition-env ::init
  [env {:keys [sim job]}]
    (onyx/init job))

(defmethod onyx/transition-env ::step
  [env _]
  (step env))

(defmethod onyx/transition-env ::tick
  [env _]
  (onyx/tick env))

(defmethod onyx/transition-env ::drain
  [env _]
  (onyx/drain env))

(defmethod onyx/transition-env ::remove-segment
  [env {:keys [task segment]}]
  (update-in env [:tasks task :outputs] (partial into [] (remove #(= segment %)))))

(defmethod onyx/transition-env ::new-segment
  [env {:keys [task segment]}]
  (onyx/new-segment env task segment))

(defn- onyx-feed-loop
  "EXPERIMENTAL"
  [env & selections]
  ;; TODO: :in and :render need to be genericized
  (reduce
    (fn [env selection]
      (-> (onyx/new-segment env :in selection)
          (remove-segment :render selection)))
    env
    selections))
