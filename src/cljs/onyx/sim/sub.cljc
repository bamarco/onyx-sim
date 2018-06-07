(ns onyx.sim.sub
  (:require
    [onyx.sim.kb :as kb]
    [onyx.sim.utils :refer [cat-into]]))

(def ?settings
  '{:onyx.sim.kb/type :onyx.sim.kb.datascript/pull
    :onyx.sim.kb.datascript/pull-expr [*]
    :onyx.sim.kb/in {$ :db}
    :onyx.sim.kb.datascript/eid [:onyx.sim.ui/name :onyx.sim.ui/settings]})

(def ?jobs
  '{:onyx.sim.kb/type :onyx.sim.kb.datascript/q
    :onyx.sim.kb.datascript/find [?job-id]
    :onyx.sim.kb/in {$ :db}
    :onyx.sim.kb.datascript/where [[_ :onyx/job-id ?job-id]]})

(def ?job-expr
  '{:onyx.sim.kb/type :onyx.sim.kb.datascript/pull
    :onyx.sim.kb.datascript/pull-expr ?expr
    :onyx.sim.kb.datascript/eid [:onyx/job-id ?job-id]
    :onyx.sim.kb/in {$ :db
                     ?job-id :onyx.sim.ui/job-id
                     ?expr :onyx.sim.ui/expr}})

(def ?animating
  '{:onyx.sim.kb/type :onyx.sim.kb.datascript/q
    :onyx.sim.kb.datascript/find [?job .]
    :onyx.sim.kb/in {$ :db}
    :onyx.sim.kb.datascript/where [[?job :onyx.sim.ui/animating? true]]})

(def ?hidden-tasks
  '{:onyx.sim.kb/type :onyx.sim.kb.datascript/pull
    :onyx.sim.kb.datascript/pull-expr [:onyx.sim.ui/hidden-tasks]
    :onyx.sim.kb.datascript/eid [:onyx/job-id ?job-id]
    :onyx.sim.kb/in {$ :db
                     ?job-id :onyx.sim.ui/job-id}})

(def ?import-uris
  '{:onyx.sim.kb/type :onyx.sim.kb.datascript/pull
    :onyx.sim.kb.datascript/pull-expr [:onyx.sim.ui/import-uris]
    :onyx.sim.kb.datascript/eid [:onyx/job-id ?job-id]
    :onyx.sim.kb/in {$ :db
                     ?job-id :onyx.sim.ui/job-id}})

(def ?sorted-tasks
  '{:onyx.sim.kb/type :onyx.sim.kb.ratom/cursor
    :onyx.sim.kb/in {$ :state
                     ?job-id :onyx.sim.ui/job-id}
    :onyx.sim.kb.ratom/path [:envs ?job-id :sorted-tasks]})

(def ?env
  '{:onyx.sim.kb/type :onyx.sim.kb.ratom/cursor
    :onyx.sim.kb/in {$ :state
                     ?job-id :onyx.sim.ui/job-id}
    :onyx.sim.kb.ratom/path [:envs ?job-id]})

;;;
;;; Subsription Functions
;;;
;;;   Eventual Goals:
;;;     * More data driven. These fns should eventually look as much like the queries above as possible
;;;     * Flexible. These fns should work with both queries and subscriptions
;;;     * Cacheing. The knowledge-base and the Knowledge-base-state should be able to build up a cache of resolved subscription functions. Eventually this could allow for reactions in the event queue to occur with reordering of events.
(defn- sorted-task-labels [knowbase]
  (let [sorted-tasks @(kb/sub knowbase ?sorted-tasks)]
    (vec
      (for [task-name sorted-tasks]
        {:id task-name
         :label (pr-str task-name)}))))

(defn- nav-choices [knowbase]
  (let [jobs @(kb/sub knowbase ?jobs)
        sims (for [[job-id] jobs]
               {:id job-id
                :label (or (:onyx.sim.ui/title @(kb/sub knowbase ?job-expr :onyx.sim.ui/expr [:onyx.sim.ui/title] :onyx.sim.ui/job-id job-id)) job-id)})]
    (cat-into
     [{:id :onyx.sim.ui/settings
       :label [:i {:class "zmdi zmdi-settings"}]}]
     sims
     [{:id :onyx.sim.ui/jobs
       :label [:i {:class "zmdi zmdi-widgets"}]}
      {:id :onyx.sim.ui/db-view
       :label [:i {:class "zmdi zmdi-assignment"}]}])))