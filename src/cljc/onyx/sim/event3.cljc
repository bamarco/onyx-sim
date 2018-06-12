(ns onyx.sim.event3
  (:require
    [onyx.sim.components.sim :as sim]
    [datascript.core :as d]
    [onyx.sim.utils :refer [gen-uuid]]
    [onyx.sim.examples.hello :as example.hello]
    [onyx.sim.examples.flow-short-circuit :as example.flow]))

;;;
;;; Helper fns
;;;
(defn ensure-sim-id 
  "Ensures the job is uniquely identified for the job-catalog"
  [job]
  ;; ???: hash off title?
  ;; ???: keywords or uuids?
  (update job :onyx.sim.api/job-id #(or % (gen-uuid))))

;;;
;;; Knowledge Base fns
;;;
(defn tx!
  "Simplified datascript transaction"
  [kb txs]
  (d/transact! (get-in kb [:datascript :conn]) (into [] txs)))

(defn tss!
  "Simplified simulator transitions"
  [kb tsses]
  (doseq [tss tsses]
    (sim/transition! (:sim kb) tss)))

(defn submit-jobs! 
  "Simplified simulator job submission"
  [kb jobs]
  (doseq [job jobs]
    (sim/submit-job (:sim kb) job)))

(defn record-jobs!
  "Record a job into the job-catalog"
  [kb jobs]
  (tx! kb (mapv ensure-sim-id jobs)))

;;;
;;; DB fns
;;;
(defn hide-task 
  "Hide the given task from the ui env display"
  [db selected-job task-name]
  (let [{:onyx.sim.ui/keys [hidden-tasks]} (d/pull db [:onyx.sim.ui/hidden-tasks] [:onyx/job-id selected-job])]
    [[:db/add 
      [:onyx/job-id selected-job] 
      :onyx.sim.ui/hidden-tasks
      (disj hidden-tasks task-name)]]))

;;;
;;; Handlers
;;;
(defmulti handle!
  "Simple handler for all onyx sim events. If the intent of events becomes more complex we will need to transition to a full state machine. Onyx itself is a good candidate for state machine management"
  (fn [knowbase event]
    (:onyx.sim.event/intent event)))

(defmethod handle! :onyx.sim.event/init-examples
  [kb _]
  (let [jobs [example.hello/job example.flow/job]]
    (submit-jobs! kb jobs)
    (record-jobs! kb jobs)))

(defmethod handle! :onyx.sim.event/init-ui
  [kb {:keys [base-ui]}]
  (tx! kb base-ui))

(defmethod handle! :onyx.sim.event/tick
  [kb {:as event :keys [selected-job]}]
  (tss! kb
    [{:onyx/job-id selected-job
      :onyx.sim.api/event :onyx.sim.api/go-tick}]))

(defmethod handle! :onyx.sim.event/step
  [kb {:as event :keys [selected-job]}]
  (tss! kb
    [{:onyx/job-id selected-job
      :onyx.sim.api/event :onyx.sim.api/go-step}]))

(defmethod handle! :onyx.sim.event/drain
  [kb {:as event :keys [selected-job]}]
  (tss! kb
    [{:onyx/job-id selected-job
      :onyx.sim.api/event :onyx.sim.api/go-drain}]))

(defmethod handle! :onyx.sim.event/play
  [kb {:as event :keys [selected-job]}]
  (tss! kb
    [{:onyx/job-id selected-job
      :onyx.sim.api/event :onyx.sim.api/go-step-drain}]))

(defmethod handle! :onyx.sim.event/hide-task
  [kb {:as event :keys [selected-job task-name]}]
  (tx! kb
    [[:db.fn/call hide-task selected-job task-name]]))

(defmethod handle! :onyx.sim.event/eav
  [kb {:as event :keys [e a v]}]
  (tx! kb
    [[:db/add e a v]]))
