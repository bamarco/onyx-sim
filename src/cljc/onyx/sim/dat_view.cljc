(ns onyx.sim.dat-view
  (:require [taoensso.timbre :as log]
            [onyx.sim.api :as onyx]
            [onyx.sim.flui :as flui]
            [datascript.core :as d]
            [onyx.sim.core :as sim]
            #?(:cljs [posh.reagent :as posh])
            #?(:cljs [reagent.core :as r :refer [atom]])))

;;;
;;; Predicates
;;;
(defn ^:export represent? [event old-seg seg all-new represent]
  (log/info "represent?" represent (= (:dat.view/represent seg) represent))
  (= (:dat.view/represent seg) represent))

;;;
;;; Helpers
;;;
(defn ^:export map-alias [alias-map seg]
  (into
    seg
    (map
      (fn [[alias-attr path]]
        [alias-attr (get-in seg path)]))
    alias-map))

(defn ^:export with-context [{:as event :keys [dat.view/alias]} seg]
  (into
    event
    (map-alias alias seg)))

(defn parse-find-vars [q-expr]
  ;; TODO: implement. for now a hardcoded value for testing
  '[:?todo])

;;;
;;; Event
;;;
(defn ^:export simple-toggle [db {:keys [dat.view/entity dat.view/attr dat.view/inputs]}]
  (let [{:as e :keys [db/id]} (d/entity db entity)
        v (get e attr)]
    (log/info "toggle" [id attr] "from" v "to" (not v))
    [[:db/retract id attr v]
     [:db/add id attr (not v)]]))

(defn ^:export simple-value [db {:keys [dat.view/entity dat.view/attr dat.view/inputs]}]
  (let [{:as e :keys [db/id]} (d/entity db entity)
        old-v (get e attr)
        new-v (first inputs)]
    (log/info "change" [id attr] "from" old-v "to" new-v)
    [[:db/retract id attr old-v]
     [:db/add id attr new-v]]))

;;;
;;; Lifecycle
;;;
(def conn-context
  (sim/system-contexter
   {:dat.sync/conn [:app-root :conn]}))

(def dispatch-context
  (sim/system-contexter
    {:dat.view/dispatch! [:app-root :dispatch!]}))

(def ^:export dat-view-lifecycle
  {:lifecycle/before-task-start (sim/context-injecter conn-context dispatch-context)})

(defn ^:export sim-render [outputs]
  [flui/h-box
   :children
   (mapv :dat.view/component outputs)])

;;;
;;; Onyx render
;;;
(defn render-segment [conn sim-id seg]
  (log/info "rendering seg" seg)
  (let [env (sim/pull-clean-env conn sim-id)]
    (try
      (-> (onyx/new-segment env :dat.view/render seg)
          onyx/drain
          :tasks
          :dat.view/mount
          :outputs
          first
          :dat.view/component)
      (catch #?(:clj Error :cljs :default) e
        [sim/sim-debug (onyx/new-segment env :dat.view/render seg)]))))

(defn render-segments->debug-sim [db parent-sim-id segs child-name]
  (let [parent-sim (d/entity db parent-sim-id)]
    (log/info "parent job" (:onyx.core/job parent-sim))
  [(into
     sim/default-sim
     {:onyx.sim/title child-name
      :onyx/type :onyx.sim/sim
      :onyx.core/job (get-in parent-sim [:onyx.core/job :db/id])
      :onyx.sim/env (reduce #(onyx/new-segment %1 :dat.view/render %2) (:onyx.sim/clean-env parent-sim) segs)
      :onyx.sim/clean-env (:onyx.sim/clean-env parent-sim)})]))

(defn box* [{:keys [dat.sync/conn]}
                           {:as seg :keys [dat.view/direction dat.view/layout dat.view/style]}]
  (log/info "dat-view-box" layout)
  (let [sim-id [:onyx/name :dat.view/sim]
        children (map
                   (fn [item]
                     [render-segment conn sim-id item])
                   layout)]
    ;; FIXME: hardcoded sim-id
    [flui/v-box
     :children
     [[flui/button
       :label "Convert to Simulator"
       :on-click #(d/transact! conn [[:db.fn/call render-segments->debug-sim sim-id layout "Spawned Sim"]])]
      [(case direction
         :horizontal flui/h-box
         flui/v-box)
       :style style
       :children (vec children)]]]))

;;;
;;; View Component Tasks
;;;
(defn ^:export box [sys seg]
  (assoc
    seg
    :dat.view/component
    [box* sys seg]))

(defn ^:export label [sys {:as seg :keys [dat.view/label]}]
  (assoc
    seg
    :dat.view/component
    [flui/label :label label]))

(defn ^:export checkbox [{:as sys :keys [dat.view/dispatch!]}
                                  {:as seg :keys [dat.view/label
                                                  dat.view/toggled?
                                                  dat.view/event]}]
  (let [default-event {:dat.view/handler ::simple-toggle
                       :dat.view/entity (:db/id seg)
                       :dat.view/attr :dat.view/toggled?}]
    (assoc
      seg
      :dat.view/component
      [flui/checkbox
       :model toggled?
       :label label
       :on-change #(dispatch! (into default-event (with-context event seg)))])))

(defn ^:export text-input [{:as sys :keys [dat.view/dispatch!]} {:as seg :keys [dat.view/label dat.view/event]}]
  (assoc
    seg
    :dat.view/component
    [flui/input-text
     :model label
     :on-change (partial dispatch! (with-context event seg))]))

(defn ^:export default [seg]
  (assoc
    seg
    :dat.view/component
    [flui/p (str "Unknown representation:" seg)]))

;;;
;;; Subscription Tasks
;;;
(defn ^:export route [{:keys [dat.sync/conn]} {:as seg :keys [dat.view/route db/id]}]
  (log/info "Routing (or " id route ")")
  (into
    seg
    (sim/pull conn '[*] (or id [:dat.view/route route]))))

(defn ^:export pull [{:as sys :keys [dat.sync/conn]}
                              {:as seg :keys [dat.view/pull-expr
                                              dat.view/entity
                                              dat.view/alias]}]
  (log/info "pull")
  (log/info pull-expr entity)
  (map-alias
    alias
    (into
      seg
      (when pull-expr
        (sim/pull conn pull-expr entity)))))

(defn ^:export query [{:as sys :keys [dat.sync/conn]}
                           {:as seg :keys [dat.view/q-expr
                                           dat.view/inputs
                                           dat.view/layout-alias
                                           dat.view/layout-value]}]
  (let [find-vars (parse-find-vars q-expr)
        relation (when q-expr
                   (apply sim/q q-expr conn inputs))]
    (into
      seg
      (when relation
        {:dat.view/layout
         (mapv
           (fn [row]
             (into
               (or layout-value {})
               (map-alias layout-alias (zipmap find-vars row))))
           relation)}))))


;;;
;;; Render Job
;;;
(defn catalog
  ([] (catalog 20))
  ([onyx-batch-size]
   [{:onyx/name :dat.view/render
     :onyx/type :input
     :onyx/batch-size onyx-batch-size}
    {:onyx/name :dat.view/dispatch
     :onyx/type :input
     :onyx/batch-size onyx-batch-size}

    {:onyx/name :dat.view.subscribe/route
     :onyx/type :function
     :onyx/fn ::route
     :onyx/batch-size onyx-batch-size}
    {:onyx/name :dat.view.subscribe/pull
     :onyx/type :function
     :onyx/fn ::pull
     :onyx/batch-size onyx-batch-size}
    {:onyx/name :dat.view.subscribe/query
     :onyx/type :function
     :onyx/fn ::query
     :onyx/batch-size onyx-batch-size}

    {:onyx/name :dat.view.represent/box
     :onyx/type :function
     :onyx/fn ::box
     :onyx/batch-size onyx-batch-size}
    {:onyx/name :dat.view.represent/label
     :onyx/type :function
     :onyx/fn ::label
     :onyx/batch-size onyx-batch-size}
    {:onyx/name :dat.view.represent/checkbox
     :onyx/type :function
     :onyx/fn ::checkbox
     :onyx/batch-size onyx-batch-size}
    {:onyx/name :dat.view.represent/text-input
     :onyx/type :function
     :onyx/fn ::text-input
     :onyx/batch-size onyx-batch-size}
    {:onyx/name :dat.view.represent/default
     :onyx/type :function
     :onyx/fn ::default
     :onyx/batch-size onyx-batch-size}

    {:onyx/name :dat.view/mount
     :onyx/type :output
     :onyx.sim/render sim-render
     :onyt/batch-size onyx-batch-size}]))

(defn job []
  {:onyx/type :onyx.core/job
   :onyx.core/catalog (catalog)

   :onyx.core/lifecycles
   [{:lifecycle/task :dat.view.subscribe/route
     :lifecycle/calls ::dat-view-lifecycle
     :onyx.sim/system :onyx.sim.system/system}
    {:lifecycle/task :dat.view.subscribe/pull
     :lifecycle/calls ::dat-view-lifecycle
     :onyx.sim/system :onyx.sim.system/system}
    {:lifecycle/task :dat.view.subscribe/query
     :lifecycle/calls ::dat-view-lifecycle
     :onyx.sim/system :onyx.sim.system/system}
    {:lifecycle/task :dat.view.represent/box
     :lifecycle/calls ::dat-view-lifecycle
     :onyx.sim/system :onyx.sim.system/system}
    {:lifecycle/task :dat.view.represent/label
     :lifecycle/calls ::dat-view-lifecycle
     :onyx.sim/system :onyx.sim.system/system}
    {:lifecycle/task :dat.view.represent/checkbox
     :lifecycle/calls ::dat-view-lifecycle
     :onyx.sim/system :onyx.sim.system/system}
    {:lifecycle/task :dat.view.represent/text-input
     :lifecycle/calls ::dat-view-lifecycle
     :onyx.sim/system :onyx.sim.system/system}]

   :onyx.core/workflow
   [[:dat.view/render :dat.view.subscribe/route]
    [:dat.view.subscribe/route :dat.view.subscribe/query]
    [:dat.view.subscribe/query :dat.view.subscribe/pull]

    [:dat.view.subscribe/pull :dat.view.represent/default]
    [:dat.view.subscribe/pull :dat.view.represent/label]
    [:dat.view.subscribe/pull :dat.view.represent/checkbox]
    [:dat.view.subscribe/pull :dat.view.represent/text-input]
    [:dat.view.subscribe/pull :dat.view.represent/box]

    [:dat.view.represent/default :dat.view/mount]
    [:dat.view.represent/label :dat.view/mount]
    [:dat.view.represent/checkbox :dat.view/mount]
    [:dat.view.represent/text-input :dat.view/mount]
    [:dat.view.represent/box :dat.view/mount]]

   :onyx.core/flow-conditions
   [{:flow/from :dat.view.subscribe/pull
     :flow/to [:dat.view.represent/box]
     :dat.view/represent :dat.view.represent/box
     :flow/predicate [::represent? :dat.view/represent]
     :flow/short-circuit? true}
    {:flow/from :dat.view.subscribe/pull
     :flow/to [:dat.view.represent/label]
     :dat.view/represent :dat.view.represent/label
     :flow/predicate [::represent? :dat.view/represent]
     :flow/short-circuit? true}
    {:flow/from :dat.view.subscribe/pull
     :flow/to [:dat.view.represent/checkbox]
     :dat.view/represent :dat.view.represent/checkbox
     :flow/predicate [::represent? :dat.view/represent]
     :flow/short-circuit? true}
    {:flow/from :dat.view.subscribe/pull
     :flow/to [:dat.view.represent/text-input]
     :dat.view/represent :dat.view.represent/text-input
     :flow/predicate [::represent? :dat.view/represent]
     :flow/short-circuit? true}
    {:flow/from :dat.view.subscribe/pull
     :flow/to [:dat.view.represent/default]
     :flow/predicate :onyx.sim.core/always
     :flow/short-circuit? true}]})

(defn simulator []
  (sim/make-sim
    :name :dat.view/sim
    :title "Dat View Simulator"
    :description "This will simulate the compute graph for dat view."
    :job (job)))

(def todos-query
  '[:find ?todo
    :in $
    :where
    [?todo :e/type :e.type/Todo]])

(defn example []
   [{:e/type :e.type/Todo
     :e/name "Test Todo"
     :dat.view/toggled? false}
    {:e/type :e.type/Todo
     :e/name "Bake a Cake"
     :dat.view/toggled? true}
    {:dat.view/route :dat.view/hello-world
     :dat.view/represent :dat.view.represent/label
     :dat.view/label "Hello, World!"}
    {:dat.view/route :dat.view/bye-world
     :dat.view/represent :dat.view.represent/text-input
     :dat.view/event {:dat.view/handler ::simple-value
                      :dat.view/entity [:dat.view/route :dat.view/bye-world]
                      :dat.view/attr :dat.view/label}
     :dat.view/label "Goodbye, World!"}
    {:dat.view/route :dat.view/bye-world2
     :dat.view/subscribe :dat.view.subscribe/pull
     :dat.view/represent :dat.view.represent/label
     :dat.view/pull-expr [:dat.view/label]
     :dat.view/entity [:dat.view/route :dat.view/bye-world]}
    {:dat.view/route :dat.view.route/index
     :dat.view/style {:background-color :LightGray}
     :dat.view/direction :vertical
     :dat.view/represent :dat.view.represent/box
     :dat.view/layout [[:dat.view/route :dat.view/hello-world]
                       [:dat.view/route :dat.view/bye-world]
                       [:dat.view/route :dat.view/bye-world2]]}
    {:dat.view/route :dat.view.route/todos
     :dat.view/subscribe :dat.view.subscribe/q
     :dat.view/represent :dat.view.represent/box
     :dat.view/q-expr todos-query
     :dat.view/layout-value {:dat.view/route :dat.view.route/todo}
     :dat.view/layout-alias '{:dat.view/entity [:?todo]}}
    {:dat.view/route :dat.view.route/todo
     :dat.view/subscribe :dat.view.subscribe/pull
     :dat.view/represent :dat.view.represent/checkbox
     :dat.view/pull-expr `[:e/name :dat.view/toggled?]
     :dat.view/alias {:dat.view/label [:e/name]}
     ;; Note: this event is optional since it is identical to the default event for checkbox
     :dat.view/event {:dat.view/handler ::simple-toggle
                      :dat.view/attr :dat.view/toggled?
                      :dat.view/alias {:dat.view/entity [:db/id]}}}])
