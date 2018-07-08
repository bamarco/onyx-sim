(ns onyx.sim.console.event
  (:require
    [taoensso.timbre :as log]
    [onyx.sim.components.sim :as sim]
    [datascript.core :as d]
    [onyx.sim.kb :refer [q snap]]
    [onyx.sim.utils :refer [gen-uuid]]
    [onyx.sim.console.subscription :as sub]
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
  (let [{:onyx.sim.console.ui/keys [hidden-tasks]} (d/pull db [:onyx.sim.console.ui/hidden-tasks] [:onyx/job-id selected-job])]
    [[:db/add 
      [:onyx/job-id selected-job] 
      :onyx.sim.console.ui/hidden-tasks
      (disj hidden-tasks task-name)]]))

;;;
;;; Handlers
;;;
(defmulti handle!
  "Simple handler for all onyx sim events. If the intent of events becomes more complex we will need to transition to a full state machine. Onyx itself is a good candidate for state machine management"
  (fn [knowbase event]
    (:onyx.sim.event/intent event)))

(defmethod handle! ::init-examples
  [kb _]
  (let [jobs [example.hello/job example.flow/job]]
    (submit-jobs! kb jobs)
    (record-jobs! kb jobs)))

(defmethod handle! ::init-ui
  [kb {:keys [base-ui]}]
  (tx! kb base-ui))

(defmethod handle! ::tick
  [kb {:as event :keys [selected-job]}]
  (tss! kb
    [{:onyx/job-id selected-job
      :onyx.sim.api/event :onyx.sim.api/go-tick}]))

(defmethod handle! ::step
  [kb {:as event :keys [selected-job]}]
  (tss! kb
    [{:onyx/job-id selected-job
      :onyx.sim.api/event :onyx.sim.api/go-step}]))

(defmethod handle! ::drain
  [kb {:as event :keys [selected-job]}]
  (tss! kb
    [{:onyx/job-id selected-job
      :onyx.sim.api/event :onyx.sim.api/go-drain}]))

(defmethod handle! ::play
  [kb {:as event :keys [selected-job]}]
  (tss! kb
    [{:onyx/job-id selected-job
      :onyx.sim.api/event :onyx.sim.api/go-step-drain}]))

(defmethod handle! ::hide-task
  [kb {:as event :keys [selected-job task-name]}]
  (tx! kb
    [[:db.fn/call hide-task selected-job task-name]]))

(defmethod handle! ::submit-job
  [kb {:as event :onyx.sim.console.ui/keys [job-id]}]
  (let [snapshot (snap kb)
        _ (log/info "snapshot" snapshot)
        job (q snapshot sub/?job-entry :onyx.sim.api/job-id job-id)]
    (submit-jobs! kb [job])))

(defmethod handle! ::eav
  [kb {:as event :keys [e a v]}]
  (tx! kb
    [[:db/add e a v]]))
