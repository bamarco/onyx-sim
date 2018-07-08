(ns onyx.sim.console.subscription
  (:require
    [onyx.sim.kb :refer [sub q]]
    [onyx.sim.utils :refer [cat-into]]))

(def ?settings
  '{:onyx.sim.kb/type :onyx.sim.kb.datascript/pull
    :onyx.sim.kb.datascript/pull-expr [*]
    :onyx.sim.kb/in {$ :datascript}
    :onyx.sim.kb.datascript/eid [:onyx.sim.console.ui/name :onyx.sim.console.ui/settings]})

(def ?jobs
  '{:onyx.sim.kb/type :onyx.sim.kb.datascript/q
    :onyx.sim.kb.datascript/find [?job-id]
    :onyx.sim.kb/in {$ :datascript}
    :onyx.sim.kb.datascript/where [[_ :onyx/job-id ?job-id]]})

(def ?active-jobs
  '{:onyx.sim.kb/type :onyx.sim.kb.datascript/q
    :onyx.sim.kb.datascript/find [?job-id]
    :onyx.sim.kb/in {$ :datascript}
    :onyx.sim.kb.datascript/where [[_ :onyx.sim.console.ui/job-id ?job-id]]})

(def ?job-catalog
  '{:onyx.sim.kb/type :onyx.sim.kb.datascript/q
    :onyx.sim.kb/in {$ :datascript}
    :onyx.sim.kb.datascript/find [?job-id]
    :onyx.sim.kb.datascript/where [[_ :onyx.sim.api/job-id ?job-id]]})

(def ?job-entry
  '{:onyx.sim.kb/type :onyx.sim.kb.datascript/pull
    :onyx.sim.kb/in {$ :datascript
                     ?job-id :onyx.sim.api/job-id}
    :onyx.sim.kb.datascript/pull-expr [{:onyx.core/catalog [*]}
                                       *]
    :onyx.sim.kb.datascript/eid [:onyx.sim.api/job-id ?job-id]})

(def ?job-expr
  '{:onyx.sim.kb/type :onyx.sim.kb.datascript/pull
    :onyx.sim.kb.datascript/pull-expr ?expr
    :onyx.sim.kb.datascript/eid [:onyx.sim.api/job-id ?job-id]
    :onyx.sim.kb/in {$ :datascript
                     ?job-id :onyx.sim.console.ui/job-id
                     ?expr :onyx.sim.console.ui/expr}})

(def ?animating
  '{:onyx.sim.kb/type :onyx.sim.kb.datascript/q
    :onyx.sim.kb.datascript/find [?job .]
    :onyx.sim.kb/in {$ :datascript}
    :onyx.sim.kb.datascript/where [[?job :onyx.sim.console.ui/animating? true]]})

(def ?hidden-tasks
  '{:onyx.sim.kb/type :onyx.sim.kb.datascript/pull
    :onyx.sim.kb.datascript/pull-expr [:onyx.sim.console.ui/hidden-tasks]
    :onyx.sim.kb.datascript/eid [:onyx/job-id ?job-id]
    :onyx.sim.kb/in {$ :datascript
                     ?job-id :onyx.sim.console.ui/job-id}})

; (def ?import-uris
;   '{:onyx.sim.kb/type :onyx.sim.kb.datascript/pull
;     :onyx.sim.kb.datascript/pull-expr [:onyx.sim.console.ui/import-uris]
;     :onyx.sim.kb.datascript/eid [:onyx/job-id ?job-id]
;     :onyx.sim.kb/in {$ :datascript
;                      ?job-id :onyx.sim.console.ui/job-id}})

; (def ?sorted-tasks
;   '{:onyx.sim.kb/type :onyx.sim.kb.ratom/cursor
;     :onyx.sim.kb/in {$ :state
;                      ?job-id :onyx.sim.console.ui/job-id}
;     :onyx.sim.kb.ratom/path [:envs ?job-id :sorted-tasks]})

; (def ?env
;   '{:onyx.sim.kb/type :onyx.sim.kb.ratom/cursor
;     :onyx.sim.kb/in {$ :state
;                      ?job-id :onyx.sim.console.ui/job-id}
;     :onyx.sim.kb.ratom/path [:envs ?job-id]})

(defn env-in
  ([kb] @(get-in kb [:sim :envs]))
  ([kb path] (get-in (env-in kb) path)))

;;;
;;; Subsription Functions
;;;
;;;   Eventual Goals:
;;;     * More data driven. These fns should eventually look as much like the queries above as possible
;;;     * Flexible. These fns should work with both queries and subscriptions
;;;     * Cacheing. The knowledge-base and the Knowledge-base-state should be able to build up a cache of resolved subscription functions. Eventually this could allow for reactions in the event queue to occur with reordering of events.
(defn sorted-task-labels [kb job-id]
  (let [sorted-tasks (env-in kb [job-id :sorted-tasks])]
    (vec
      (for [task-name sorted-tasks]
        {:id task-name
         :label (pr-str task-name)}))))

(defn jobs2 [{:as knowbase :keys [sim]}]
  (keys (:envs sim)))

(defn job-title [kb job-id]
  (let [{:onyx.sim.console.ui/keys [title]} (sub kb ?job-expr 
                                             :onyx.sim.console.ui/expr [:onyx.sim.console.ui/title]
                                             :onyx.sim.api/job-id job-id)]
    (or title (str job-id))))

(defn nav-tab-icons
  "Icons for navigation tabs"
  [kb]   
  [
    {:id :onyx.sim.console.ui/settings
     :label [:i {:class "zmdi zmdi-settings"}]}
    {:id :onyx.sim.console.ui/db-frisk
     :label [:i {:class "zmdi zmdi-cloud-box"}]}
    {:id :onyx.sim.console.ui/job-catalog
     :label [:i {:class "zmdi zmdi-widgets"}]}
    {:id :onyx.sim.console.ui/running-jobs
     :label [:i {:class "zmdi zmdi-collection-video"}]}])

(defn running-jobs 
  ""
  [kb]
  (let [jobs @(sub kb ?active-jobs)]
      ; sims (for [job-id (jobs2 kb)];[[job-id] jobs]
      ;         {:id job-id
      ;          :label (job-title kb job-id)})
    jobs))


(defn selected-nav 
  "The value for the main navigation selection"
  [kb]
  (let [{:onyx.sim.console.ui/keys [selected-nav]} @(sub kb ?settings)]
    (or selected-nav :onyx.sim.console.ui/settings)))

(defn the-whole-conn 
  "Just give me the raw conn"
  [kb]
  (get-in kb [:datascript :conn]))
