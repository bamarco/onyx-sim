(ns onyx.sim.console.subscription
  (:require
    [onyx.sim.kb :refer [sub q]]
    [onyx.sim.utils :refer [cat-into forv]]))

(def ?settings
  '{:onyx.sim.kb/type :onyx.sim.kb.datascript/pull
    :onyx.sim.kb.datascript/pull-expr [*]
    :onyx.sim.kb/in {$ :datascript}
    :onyx.sim.kb.datascript/eid [:onyx.sim.console.ui/name :onyx.sim.console.ui/settings]})

(def ?job-catalog
  '{:onyx.sim.kb/type :onyx.sim.kb.datascript/q
    :onyx.sim.kb/in {$ :datascript}
    :onyx.sim.kb.datascript/find [?job-id]
    :onyx.sim.kb.datascript/where [[_ :onyx.sim.api/catalog-id ?job-id]]})

(def ?catalog-entry
  '{:onyx.sim.kb/type :onyx.sim.kb.datascript/pull
    :onyx.sim.kb/in {$ :datascript
                     ?catalog-id :catalog-id}
    :onyx.sim.kb.datascript/pull-expr [{:onyx.core/catalog [*]}
                                       *]
    :onyx.sim.kb.datascript/eid [:onyx.sim.api/catalog-id ?catalog-id]})

(def ?job-expr
  '{:onyx.sim.kb/type :onyx.sim.kb.datascript/pull
    :onyx.sim.kb.datascript/pull-expr ?expr
    :onyx.sim.kb.datascript/eid [:onyx.sim.api/catalog-id ?job-id]
    :onyx.sim.kb/in {$ :datascript
                     ?job-id :job-id
                     ?expr :expr}})

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
                     ?job-id :job-id}})



;;;
;;; Subsription Functions
;;;
;;;   Eventual Goals:
;;;     * More data driven. These fns should eventually look as much like the queries above as possible
;;;     * Flexible. These fns should work with both queries and subscriptions
;;;     * Cacheing. The knowledge-base and the Knowledge-base-state should be able to build up a cache of resolved subscription functions. Eventually this could allow for reactions in the event queue to occur with reordering of events.
(defn env-in
  ([kb] @(get-in kb [:sim :envs]))
  ([kb path] (get-in (env-in kb) path)))

(defn master-id [kb job-id]
  (env-in [kb job-id :onyx.sim.api/master]))

(defn render-fn [kb job-id]
  (let [catalog-id (master-id kb job-id)
        {:onyx.sim.ui/keys [render]} @(sub kb ?job-expr :expr [:onyx.sim.ui/render] :job-id catalog-id)]
    render))

(defn sorted-task-labels [kb job-id]
  (let [sorted-tasks (env-in kb [job-id :sorted-tasks])]
    (vec
      (for [task-name sorted-tasks]
        {:id task-name
         :label (pr-str task-name)}))))

(defn job-title [kb job-id]
  (let [{:onyx.sim.console.ui/keys [title]} @(sub kb ?job-expr 
                                              :onyx.sim.console.ui/expr [:onyx.sim.console.ui/title]
                                              :onyx.sim.api/catalog-id job-id)]
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

(defn running-envs 
  "Jobs that are still running in the simulator"
  ([kb] (vec (vals (env-in kb))))
  ([kb master-catalog-id]
   (filterv 
    #(= (:onyx.sim.api/master %) master-catalog-id)
    (running-envs kb))))

(defn running-job-ids
  ([kb] (running-job-ids kb nil))
  ([kb master-catalog-id]
   (mapv :onyx/job-id (running-envs kb master-catalog-id))))

(defn selected-nav 
  "The value for the main navigation selection"
  [kb]
  (let [{:onyx.sim.console.ui/keys [selected-nav]} @(sub kb ?settings)]
    (or selected-nav :onyx.sim.console.ui/settings)))

(defn the-whole-conn 
  "Just give me the raw conn"
  [kb]
  (get-in kb [:datascript :conn]))
