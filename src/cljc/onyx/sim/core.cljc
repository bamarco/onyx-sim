(ns onyx.sim.core
  (:require [taoensso.timbre :as log]
            [onyx.sim.api :as onyx]
            [onyx.sim.flui :as flui]
            [onyx.sim.control :as control]
            [onyx.sim.svg :as svg]
            [clojure.core.async :as async]
            [clojure.spec.alpha :as s]
            [onyx.sim.event :as event :refer [dispatch raw-dispatch]]
            [onyx.sim.utils :as utils :refer [cat-into]]
            [datascript.core :as d]
            [onyx.sim.catalog]
            [onyx.sim.examples.flow-short-circuit]
            #?(:cljs [posh.reagent :as posh])
            #?(:cljs [reagent.core :as r :refer [atom]])))


;;; tasks
(defn ^:export hello [seg]
  (assoc seg :hello-msg "Hello, world!"))

;; NOTE: task fns must be defined before job is onyx/init or race conditions occur

;;
;; UTILS
;;
(def onyx-green "#43d16b")
(def onyx-gray "#354d56")
(def sim-blue "#6a8aac")
(def sim-pale-blue "#9ee3ff")
(def sim-gold "#c0b283")
(def sim-dark-tan "#A07F60")
(def sim-light-tan "#D7CEC7")

(def q
  #?(:cljs
      (comp deref posh/q)
      :clj
      (fn [query conn & args]
        (apply d/q query @conn args))))

(def pull
  #?(:cljs
      (comp deref posh/pull)
      :clj
      (fn [conn expr eid]
        (d/pull @conn expr eid))))

(defn pull-q [pull-expr query conn & input]
  (map (fn [[eid]] (pull conn pull-expr eid)) (apply q query conn input)))

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

(def control-catalog
  '[{:control/type :indicator
     :control/name :onyx.sim/next-action
     :control/label "Next Action"
     :control/show? (:onyx.sim.control/control-attr :onyx.sim/next-action? :control/toggled?)
     :control/display (::next-action-actual)}
    {:control/type :choice
     :control/name :onyx.sim/nav
     :control/label "Navigation"
     :control/chosen (::selected-nav)
     :control/choose (::select-view)
     :control/choices (::view-choices)}
    {:control/type :active-logo
     :control/name :onyx.sim/logo
     :control/label "nyx-sim (alpha)"
     :control/img "onyx-logo.png"
     :control/active? (::any-running?)}
    {:control/type :indicator
     :control/name :onyx.sim/description
     :control/label "Description"
     :control/show? (:onyx.sim.control/control-attr :onyx.sim/description? :control/toggled?)
     :control/display (::description-actual)}
    {:control/type :toggle
     :control/name :onyx.sim/description?
     :control/label "Show Sim Description"
     :control/toggle (:onyx.sim.control/simple-toggle :onyx.sim/description?)
     :control/toggle-event {:dat.view/handler ::simple-toggle
                            :dat.view/entity [:control/name :onyx.sim/description?]
                            :dat.view/attr :control/toggled?}
     :control/toggled? false}
    {:control/type :toggle
     :control/name :onyx.sim/hidden?
     :control/label "Show Task Hider"
     :control/toggle (:onyx.sim.control/simple-toggle :onyx.sim/hidden?)
     :control/toggle-event {:dat.view/handler ::simple-toggle
                            :dat.view/entity [:control/name :onyx.sim/hidden?]
                            :dat.view/attr :control/toggled?}
     :control/toggled? true}
    {:control/type :toggle
     :control/name :onyx.sim/next-action?
     :control/label "Show Next Action"
     :control/toggle (:onyx.sim.control/simple-toggle :onyx.sim/next-action?)
     :control/toggled? true}
    {:control/type :choice
     :control/name :onyx.sim/env-display-style
     :control/label "Environment Display Style"
     :control/chosen #{:pretty-env}
     :control/choose (:onyx.sim.control/simple-choose-one :onyx.sim/env-display-style)
     :control/choose-event {:dat.view/handler ::simple-value
                            :dat.view/entity [:control/name :onyx.sim/hidden?]
                            :dat.view/attr :control/chosen}
     :control/choices [{:id :pretty-env
                        :e/order 0
                        :label "Pretty"}
                       {:id :raw-env
                        :e/order 1
                        :label "Raw"}]}
    {:control/type :toggle
     :control/name :onyx.sim/render-segments?
     :control/label "(Render Segments)"
     :control/toggle (:onyx.sim.control/simple-toggle :onyx.sim/render-segments?)
     :control/disabled? (:onyx.sim.control/simple-not-chosen? :onyx.sim/env-display-style :pretty-env) ;; ***TODO: how to merge from shared subscription to different representation
             ;;          one is radio button one is checkbox
     :control/toggled? true}
    {:control/type :toggle
     :control/name :onyx.sim/only-summary?
     :control/label "(Only Summary)"
     :control/toggle (:onyx.sim.control/simple-toggle :onyx.sim/only-summary?)
     :control/disabled? (:onyx.sim.control/simple-not-chosen? :onyx.sim/env-display-style :raw-env)
     :control/toggled? true}
    {:control/type :action
     :control/name :onyx.api/tick
     :control/label "Tick"
     :control/action (::tick)
     :control/disabled? (::running?)}
    {:control/type :action
     :control/name :onyx.api/step
     :control/label "Step"
     :control/action (::step)
     :control/disabled? (::running?)}
    {:control/type :action
     :control/name :onyx.api/drain
     :control/label "Drain"
     :control/action (::drain)
     :control/disabled? (::running?)}
    {:control/type :toggle
     :control/name :onyx.sim/play
     :control/label "Play"
     :control/toggle-label "Pause"
     :control/toggled? (::running?)
     :control/toggle (::toggle-play)}
    {:control/type :choice
     :control/name :onyx.sim/hidden-tasks
     :control/label "Hidden Tasks"
     :control/chosen (::hidden-tasks)
     :control/choose (::hide-tasks)
     :control/choices (::sorted-tasks)
     :control/show? (:onyx.sim.control/control-attr :onyx.sim/hidden? :control/toggled?)
     :control/id-fn :onyx/name
     :control/label-fn :onyx/name}])

(def task-colors
  {:function sim-pale-blue
   :input onyx-gray
   :output onyx-green})

(def default-sim
  {:onyx/type :onyx.sim/sim
   :onyx.sim/import-uris ["route.edn" "example.edn"]
   :onyx.sim/speed 1.0
   :onyx.sim/running? false
   :onyx.sim/hidden-tasks #{}})


(def ds-schema
  {:onyx.core/catalog {:db/type :db.type/ref
                       :db/cardinality :db.cardinality/many}
   :onyx.sim.view/options {:db/type :db.type/ref
                           :db/cardinality :db.cardinality/many}
   :onyx/name {:db/unique :db.unique/identity}
   :control/name {:db/unique :db.unique/identity}
   :onyx.core/job {:db/type :db.type/ref}
   :onyx.sim/selected-env {:db/type :db.type/ref}
   :onyx.sim/env {:db/type :db.type/ref}})

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

(def onyx-batch-size 20)

(def hello-sim
  (into
    default-sim
    {:onyx/name :hello-env
     :onyx.sim/title "Hello Sim!"
     :onyx.sim/description "Simulation Example."
     :onyx.core/job {:onyx/type :onyx.core/job
                     :onyx.core/catalog [{:onyx/name :in
                                          :onyx/type :input
                                          :onyx/batch-size onyx-batch-size}
                                         {:onyx/name :out
                                          :onyx/type :output
                                          :onyx/batch-size onyx-batch-size}
                                         {:onyx/name :hello
                                          :onyx/type :function
                                          :onyx/fn ::hello
                                          :onyx/batch-size onyx-batch-size}]
                     :onyx.core/workflow [[:in :hello] [:hello :out]]
;;                      :onyx.core/flow-conditions []
                     }}))

(defn resolve-fns [seg]
  ;; TODO: walk if list keyword->fn
  ;; ???: just db fns?
  )

(defn ^:export tick-action [{:as seg :keys [db/id onyx.sim/running?]}]
  {:control/action [[:db.fn/call onyx/tick id]]
   :control/disabled? running?})

(defn ^:export rename [renames seg]
  (clojure.set/rename-keys seg renames))

(defn ^:export knowledge-query [{:keys [dat.sync/q-expr dat.sync/pull-expr dat.sync/entity dat.sync/conn dat.sync/inputs]}]
  (if q-expr
    (apply q q-expr conn inputs)
    (pull conn pull-expr entity)))

;;;
;;; dat.sync/subscription
;;;
(defn alias-in [seg alias-attr path f & args]
  (assoc
    seg
    alias-attr
    (apply f (get-in seg path) args)))

(defn derive-in [seg alias-attr f]
  (assoc
    seg
    alias-attr
    (f seg)))

(defn ^:export map-alias [alias-map seg]
  (into
    seg
    (map
      (fn [[alias-attr path]]
        [alias-attr (get-in seg path)]))
    alias-map))

(defn ^:export derive-selected? [attr path value seg]
  (alias-in
    seg
    attr
    path
    contains?
    value))

(defn ^:export not-it [attr path seg]
  (alias-in
    seg
    attr
    path
    not))

;;;
;;; dat.view.render
;;;
(defn ^:export split-attrs [{:as seg :keys [dat.view/value dat.view/path]}]
  (for [[attr sub-value] value]
    {:dat.view/path (conj path attr)
     :dat.view/value sub-value}))

(defn ^:export all-attrs [{:as seg :keys [dat.view/path]}]
  (log/info "all-attrs")
  (into seg
    (get seg (last path))))

(defn ^:export my-identity [seg]
  seg)

(defn ^:export label-view [{:as seg :keys [dat.view/value]}]
  (assoc seg
    :dat.view/component
    [flui/label :label (str value)]))

(defn ^:export checkbox-view [{:keys [dat.view/dispatch!]} {:as seg :keys [control/disabled? control/label control/toggle-label control/toggled? control/toggle-event]}]
    (assoc seg
      :dat.view/component
      [flui/checkbox
       :model toggled?
       :disabled? disabled?
       :label (if toggled? (or toggle-label label) label)
       :on-change (dispatch! toggle-event)]))

(defn ^:export default-view [seg]
  (log/info "default-view")
  (assoc seg
    :dat.view/component
    [flui/p
     (str "No matching view for:"
          seg)]))

(defn ^:export dat-sync-sub [{:as context :keys [dat.sync/sub-name]} {:as seg :keys [bidi/handler]}]
  (log/info "dat-sync-sub" sub-name seg)
  ;; !!!: don't log the context because the logger will look into the conn which contains itself in the compiled onyx env
  (assoc
    seg
    sub-name (knowledge-query (merge context seg))
    :dat.view/path [handler sub-name] ;; ???: move to bidi handler?
    ))

(defn ^:export sim-render [outputs]
  [flui/h-box
   :children
   (mapv :dat.view/component outputs)])

;;;
;;; Lifecycles
;;;
(defn system-contexter [resource-locations]
  (log/info "system-contexter" resource-locations)
  (fn [event {:as lifecycle :keys [onyx.sim/system]}]
    (into
      {}
      (map (fn [[resource location]]
             (let [value (get-in (onyx/kw->fn system) location)]
;;                (log/info "system-resource: " resource " at " location " is " value)
               [resource value])))
      resource-locations)))

(defn task-contexter [task-locations]
  (fn [{:as event :keys [onyx.core/task-map]} lifecycle]
    (into
      {}
      (map (fn [[resource location]]
             [resource (get-in task-map location)]))
      task-locations)))

(defn subscription-context [{:as event :keys [onyx.core/task-map]} lifecycle]
  (assoc
    (select-keys
      task-map
      #{:dat.sync/entity :dat.sync/pull-expr :dat.sync/q-expr :dat.sync/inputs})
    :dat.sync/sub-name (:onyx/name task-map)))

(def conn-context
  (system-contexter
   {:dat.sync/conn [:app-root :conn]}))

(def dispatch-context
  (system-contexter
    {:dat.view/dispatch! [:app-root :dispatch!]}))

(defn context-injecter [& fns]
  (fn [event lifecycle]
    {:onyx.core/params
     [(transduce
       (map #(% event lifecycle))
       merge
       {}
       fns)]}))

(def ^:export dat-sync-sub-lifecycle
  {:lifecycle/before-task-start (context-injecter subscription-context conn-context)})

(def ^:export dat-view-dispatch-lifecycle
  {:lifecycle/before-task-start (context-injecter dispatch-context)})

;;;
;;; Predicates
;;;
(defn ^:export match-any-attr? [event old-seg seg all-new attrs]
  (log/info "match-attrs" (contains? attrs (last (:dat.view/path seg))))
  (contains? attrs (last (:dat.view/path seg))))

(defn ^:export always [event old-seg seg all-new]
  (log/info "ALWAYS")
  true)

(defn ^:export match-any-spec? [event old-seg seg all-new specs]
  (some #(s/valid? % seg) specs))

;;;
;;; pull-exprs
;;;
(def toggle-pull-expr
  '[:control/label :control/toggle-label :control/toggled? :control/toggle-event])

(def choices-pull-expr
  '[:control/label :control/chosen :control/choose-event :control/choices])

;;;
;;; Render Job
;;;
(def the-catalog
  [{:onyx/name :onyx.sim.route/settings
    :onyx/type :input
    :onyx/batch-size onyx-batch-size}
   {:onyx/name :onyx.sim.sub/hidden?
    :onyx/fn ::dat-sync-sub
    :dat.sync/entity [:control/name :onyx.sim/hidden?]
    :dat.sync/pull-expr toggle-pull-expr
    :onyx/type :function
    :onyx/batch-size onyx-batch-size}
   {:onyx/name :onyx.sim.sub/description?
    :onyx/fn ::dat-sync-sub
    :dat.sync/entity [:control/name :onyx.sim/description?]
    :dat.sync/pull-expr toggle-pull-expr
    :onyx/type :function
    :onyx/batch-size onyx-batch-size}
   {:onyx/name :onyx.sim.sub/next-action?
    :onyx/fn ::dat-sync-sub
    :dat.sync/entity [:control/name :onyx.sim/next-action?]
    :dat.sync/pull-expr toggle-pull-expr
    :onyx/type :function
    :onyx/batch-size onyx-batch-size}
   {:onyx/name :onyx.sim.sub/env-display-style
    :onyx/fn ::dat-sync-sub
    :dat.sync/entity [:control/name :onyx.sim/env-display-style]
    :dat.sync/pull-expr choices-pull-expr
    :onyx/type :function
    :onyx/batch-size onyx-batch-size}
   {:onyx/name :onyx.sim.sub/pretty?
    :onyx/fn ::derive-selected?
    :dat.view/alias [:onyx.sim.sub/env-display-style :control/chosen]
    :dat.view/selection :pretty-env
    :onyx/params [:onyx/name :dat.view/alias :dat.view/selection]
    :onyx/type :function
    :onyx/batch-size onyx-batch-size}
   {:onyx/name :onyx.sim.sub/render-segments?
    :onyx/fn ::dat-sync-sub
    :dat.sync/entity [:control/name :onyx.sim/render-segments?]
    :dat.sync/pull-expr [:control/label :control/toggled? :control/toggle-event]
    :onyx/type :function
    :onyx/batch-size onyx-batch-size}
   {:onyx/name :onyx.sim.sub/render-segments?control
    :onyx/fn ::map-alias
    :dat.view/alias {:control/label [:onyx.sim.sub/render-segments? :control/label]
                     :control/toggled? [:onyx.sim.sub/render-segments? :control/toggled?]
                     :control/toggle-event [:onyx.sim.sub/render-segments? :control/toggle-event]
                     :control/disabled? [:onyx.sim.sub/not-pretty?]}
    :onyx/params [:dat.view/alias]
    :onyx/type :function
    :onyx/batch-size onyx-batch-size}
   {:onyx/name :onyx.sim.sub/not-pretty?
    :onyx/fn ::not-it
    :dat.view/alias [:onyx.sim.sub/pretty?]
    :onyx/params [:onyx/name :dat.view/alias]
    :onyx/type :function
    :onyx/batch-size onyx-batch-size}
   {:onyx/name :onyx.sim.sub/disable-render?
    :onyx/fn ::not-value
    :onyx/type :function
    :onyx/batch-size onyx-batch-size}
   {:onyx/name :dat.view.render/all-attrs
    :onyx/fn ::all-attrs
    :onyx/type :function
    :onyx/batch-size onyx-batch-size}
   {:onyx/name :dat.view.render/pull-view
    :onyx/fn ::split-attrs
    :onyx/type :function
    :onyx/batch-size onyx-batch-size}
   {:onyx/name :dat.view.render/label
    :onyx/fn ::label-view
    :onyx/type :function
    :onyx/batch-size onyx-batch-size}
   {:onyx/name :dat.view.render/checkbox
    :onyx/fn ::checkbox-view
    :onyx/type :function
    :onyx/batch-size onyx-batch-size}
   {:onyx/name :dat.view.render/default
    :onyx/fn ::default-view
    :onyx/type :function
    :onyx/batch-size onyx-batch-size}
   {:onyx/name :dat.view/mount
    :onyx/type :output
    :onyx.sim/render sim-render
    :onyx/batch-size onyx-batch-size}])

(def the-workflow
  [[:onyx.sim.route/settings :onyx.sim.sub/hidden?]
   [:onyx.sim.route/settings :onyx.sim.sub/description?]
   [:onyx.sim.route/settings :onyx.sim.sub/next-action?]
   [:onyx.sim.route/settings :onyx.sim.sub/env-display-style] [:onyx.sim.sub/env-display-style :onyx.sim.sub/pretty?] [:onyx.sim.sub/pretty? :onyx.sim.sub/not-pretty?]
   [:onyx.sim.sub/not-pretty? :onyx.sim.sub/render-segments?] [:onyx.sim.sub/render-segments? :onyx.sim.sub/render-segments?control] [:onyx.sim.sub/render-segments?control :dat.view.render/checkbox]

   [:onyx.sim.sub/hidden? :dat.view.render/all-attrs]
   [:onyx.sim.sub/description? :dat.view.render/all-attrs]
   [:onyx.sim.sub/next-action? :dat.view.render/all-attrs]
   [:onyx.sim.sub/not-pretty? :dat.view.render/all-attrs]
   [:dat.view.render/all-attrs :dat.view.render/checkbox]
   [:dat.view.render/all-attrs :dat.view.render/default]
   [:dat.view.render/pull-view :dat.view.render/label]
   [:dat.view.render/pull-view :dat.view.render/default]

   [:dat.view.render/checkbox :dat.view/mount]
   [:dat.view.render/label :dat.view/mount]
   [:dat.view.render/default :dat.view/mount]
   ])

(def the-flow
  [{:flow/from :dat.view.render/pull-view
    :flow/to [:dat.view.render/label]
    :flow/short-circuit? true
    :attrs #{:e/name}
    :flow/predicate [::match-any-attr? :attrs]}
   {:flow/from :dat.view.render/pull-view
    :flow/to [:dat.view.render/default]
    :flow/short-circuit? true
    :flow/predicate ::always}
   {:flow/from :dat.view.render/all-attrs
    :flow/to [:dat.view.render/checkbox]
    :flow/short-circuit? true
    :attrs #{:onyx.sim.sub/hidden? :onyx.sim.sub/description? :onyx.sim.sub/next-action?}
    :flow/predicate [::match-any-attr? :attrs]}
   {:flow/from :dat.view.render/all-attrs
    :flow/to [:dat.view.render/default]
    :flow/short-circuit? true
    :flow/predicate ::always}])

(def the-life
  [{:lifecycle/task :onyx.sim.sub/hidden?
    :lifecycle/calls ::dat-sync-sub-lifecycle
    :onyx.sim/system :onyx.sim.system/system}
   {:lifecycle/task :onyx.sim.sub/description?
    :lifecycle/calls ::dat-sync-sub-lifecycle
    :onyx.sim/system :onyx.sim.system/system}
   {:lifecycle/task :onyx.sim.sub/next-action?
    :lifecycle/calls ::dat-sync-sub-lifecycle
    :onyx.sim/system :onyx.sim.system/system}
   {:lifecycle/task :onyx.sim.sub/env-display-style
    :lifecycle/calls ::dat-sync-sub-lifecycle
    :onyx.sim/system :onyx.sim.system/system}
   {:lifecycle/task :onyx.sim.sub/render-segments?
    :lifecycle/calls ::dat-sync-sub-lifecycle
    :onyx.sim/system :onyx.sim.system/system}
   {:lifecycle/task :dat.view.render/checkbox
    :lifecycle/calls ::dat-view-dispatch-lifecycle
    :onyx.sim/system :onyx.sim.system/system}])

(def the-job
  {:onyx/type :onyx.core/job
   :onyx.core/catalog the-catalog
   :onyx.core/workflow the-workflow
   :onyx.core/flow-conditions the-flow
   :onyx.core/lifecycles the-life})

(def site-layout
  {"/"
   [{"/bar"
     ["/active-logo"
      {"/nav"
       ["/nav/settings"
        "/nav/sim/...sim-id"
        "/nav/sim-manager"]}]}
    {"/selected"
     ["/sim/%selected"]
     "/sim/%sim-id"
     ["/sim/%sim-id/task-hider"
      {"/sim/%sim-id/action"
       ["/sim/%sim-id/action/tick"
        "/sim/%sim-id/action/step"
        "/sim/%sim-id/action/drain"
        "/sim/%sim-id/action/toggle-play"]}
      "/sim/%sim-id/next-action"
      {"/sim/%sim-id/task"
       ["/sim/%sim-id/task/...task-id"]
       "/sim/%sim-id/tasks"
       ["/sim/%sim-id/task/...task-id"]}]
     "/settings"
     ["/settings/title"
      "/settings/show-task-hider"
      "/settings/show-sim-description"
      "/settings/show-next-action"
      {"/settings/env-display-style"
       ["/settings/env-display-style/title"
        "/settings/env-display-style/pretty"
        "/settings/env-display-style/render-segments"
        "/settings/env-display-style/raw"
        "/settings/env-display-style/only-summary"]}]
     "/sim-manager"
     [{"/sim-manager/..."
       ["/sim/%sim/title"
        "/sim/%sim/description"
        "/sim-manager/add"]}]}]})

(def render-sim
  (into
    default-sim
    {:onyx/name :render-example-env
     :onyx.sim/title "Render Example"
     :onyx.sim/description "Sandbox to create new dat.view components"
     :onyx.core/job the-job}))

(defn init-job [{:as sim :keys [onyx.core/job]}]
  (assoc
    sim
    :onyx.sim/env (onyx/init (ds->onyx job))))

(defn make-sim [& {:as options}]
  (init-job
    (into
      default-sim
      (clojure.set/rename-keys options {:job :onyx.core/job
                                        :name :onyx/name
                                        :title :onyx.sim/title
                                        :description :onyx.sim/description}))))

(def base-ui
  (into
    control-catalog
    [(init-job hello-sim)
     (init-job render-sim)
     (make-sim
       :name :flow-short-circuit
       :job onyx.sim.examples.flow-short-circuit/job
       :title "Flow Short Circuit"
       :description (:onyx/doc onyx.sim.examples.flow-short-circuit/job))
     {:onyx/name :onyx.sim/settings
      :onyx.sim/selected-view :onyx.sim/selected-env
      :onyx.sim/selected-env [:onyx/name :render-example-env]}]))

(defn selected-sim [conn]
  (let [{{:keys [:db/id]} :onyx.sim/selected-env} (pull conn '[:onyx.sim/selected-env] [:onyx/name :onyx.sim/settings])]
    id))

;;
;; VIEWS
;;
(defn pretty-outbox [conn & {:keys [task-name render]}]
  (let [sim-id (selected-sim conn)
        {{tasks :tasks} :onyx.sim/env}
        (pull conn
              '[{:onyx.sim/env [*]}] sim-id)
        outputs (get-in tasks [task-name :outputs])]
    ;; ???: dump segments button
    ;; ???: feedback segments
    (when outputs
      [flui/v-box
        :class "onyx-outbox"
        :children
        [[flui/title
           :label "Outbox"
           :level :level3]
         [render outputs]]])))

(defn pretty-inbox [conn & {:keys [task-name render]}]
  (let [sim-id (selected-sim conn)
        {:keys [onyx.sim/import-uris]
         {tasks :tasks} :onyx.sim/env}
        (pull conn '[:onyx.sim/import-uris
                     {:onyx.sim/env [*]}] sim-id)
        inbox (get-in tasks [task-name :inbox])]
    [flui/v-box
     :class "onyx-inbox"
     :children
     [[flui/h-box
       :gap ".5ch"
       :align :center
       :children
       [[flui/title
         :label "Inbox"
         :level :level3]
        [flui/input-text
         :model (str (first import-uris))
         :on-change #(dispatch conn {:onyx/type :onyx.sim.event/import-uri
                                     :onyx.sim/sim sim-id
                                     :onyx.sim/task-name task-name
                                     :uri %})]
        [flui/button
         :label "Import Segments"
         :on-click #(raw-dispatch conn {:onyx/type :onyx.sim.event/import-segments
                                        :onyx.sim/sim sim-id
                                        :onyx.sim/task-name task-name})]]]
      [render inbox]]]))

(def code-render (partial flui/code :code))

(defn pretty-task-box [conn task-name]
  (let [sim-id (selected-sim conn)
        {:keys [onyx.sim/render]
         {tasks :tasks} :onyx.sim/env}
        (pull conn '[:onyx.sim/render {:onyx.sim/env [*]}] sim-id)
        task-type (get-in tasks [task-name :event :onyx.core/task-map :onyx/type])
        local-render (get-in tasks [task-name :event :onyx.core/task-map :onyx.sim/render])
        render-segments? (control/control-attr conn :onyx.sim/render-segments? :control/toggled?)
        render-fn (if render-segments?
                    (or local-render render code-render)
                    code-render)]

    (flui/v-box
      :class "onyx-task onyx-panel"
      :gap ".25rem"
      :children
      [(flui/h-box
         :gap ".5ch"
         :align :center
         :style {:background-color (get task-colors task-type)
                 :border-radius :5px}
         :children
         [(flui/gap :size ".5ch")
          (flui/title
            :label task-name
            :level :level2)
          (flui/button
            :label "Hide"
            :on-click #(dispatch conn {:onyx/type :onyx.sim.event/hide-task
                                  :onyx.sim/sim sim-id
                                  :onyx.sim/task-name task-name}))])
       [pretty-inbox conn
        :task-name task-name
        :render render-fn]
       [pretty-outbox conn
        :task-name task-name
        :render render-fn]
       ])))

(defn pretty-env [conn]
  (let [sim-id (selected-sim conn)
        {hidden                :onyx.sim/hidden-tasks
         {sorted-tasks :sorted-tasks} :onyx.sim/env}
        (pull conn '[:onyx.sim/hidden-tasks
                     {:onyx.sim/env
                      [:sorted-tasks]}] sim-id)]
    (flui/v-box
      :class "onyx-env"
      :children
      (into
        []
        (for [task-name (remove (or hidden #{}) sorted-tasks)]
          ^{:key (:onyx/name task-name)}
          [pretty-task-box conn task-name])))))

(defn summary
  ([conn] (summary conn nil))
  ([conn summary-fn]
   (let [sim-id (selected-sim conn)
         summary-fn (or summary-fn onyx/env-summary)
         {:keys [:onyx.sim/env]} (pull conn '[{:onyx.sim/env [*]}] sim-id)]
     (flui/code :class "onyx-panel" :code (summary-fn env)))))

(defn raw-env [conn]
  (let [only-summary? (control/control-attr conn :onyx.sim/only-summary? :control/toggled?)]
    (flui/v-box
      :class "onyx-env"
      :children
      [(flui/title
         :label "Raw Environment"
         :level :level3)
       [summary conn (when-not only-summary? identity)]
       ])))

;;; control-fns
(defn ^:export selected-view [conn]
  (let [{:keys [:onyx.sim/selected-view]} (pull conn '[:onyx.sim/selected-view] [:onyx/name :onyx.sim/settings])]
    selected-view))

(defn ^:export selected-nav [conn]
  (let [view (selected-view conn)]
    (if (= view :onyx.sim/selected-env)
      (selected-sim conn)
      view)))

(defn ^:export toggle-play [conn]
  #(event/raw-dispatch conn {:onyx/type :onyx.sim/toggle-play
                             :onyx.sim/sim (selected-sim conn)}))

(defn ^:export running? [conn]
  (:onyx.sim/running? (pull conn '[:onyx.sim/running?] (selected-sim conn))))

(defn ^:export tick [conn]
  #(event/dispatch conn {:onyx/type :onyx.sim/transition-env
                         :onyx.sim/transition :onyx.api/tick
                         :onyx.sim/sim (selected-sim conn)}))

(defn ^:export step [conn]
  #(event/dispatch conn {:onyx/type :onyx.sim/transition-env
                         :onyx.sim/transition :onyx.api/step
                         :onyx.sim/sim  (selected-sim conn)}))

(defn ^:export drain [conn]
  #(event/dispatch conn {:onyx/type :onyx.sim/transition-env
                         :onyx.sim/transition :onyx.api/drain
                         :onyx.sim/sim (selected-sim conn)}))

(defn ^:export next-action-actual [conn]
  (let [{{:keys [next-action]} :onyx.sim/env} (pull conn '[{:onyx.sim/env [:next-action]}] (selected-sim conn))]
    next-action))

(defn ^:export description-actual [conn]
  (let [{:keys [:onyx.sim/description]}
        (pull conn '[:onyx.sim/description] (selected-sim conn))]
    description))

(defn ^:export hidden-tasks [conn]
  (:onyx.sim/hidden-tasks (pull conn '[:onyx.sim/hidden-tasks] (selected-sim conn))))

(defn ^:export hide-tasks [conn]
  #(event/dispatch conn {:onyx/type :onyx.sim.event/hide-tasks
                         :onyx.sim/sim (selected-sim conn)
                         :onyx.sim/task-names %}))

(defn ^:export select-view [conn]
  #(event/dispatch conn {:onyx/type :onyx.sim/select-view
                         :onyx.sim/sim (selected-sim conn)
                         :selected %}))

(defn ^:export view-choices [conn]
    (let [sims (q '[:find ?title ?sim
                    :in $
                    :where
                    [?sim :onyx/name ?sim-name] ;; ???: needed?
                    [?sim :onyx.sim/title ?title]
                    [?sim :onyx/type :onyx.sim/sim]] conn)
          sims (for [[nam id] sims]
                 {:id id
                  :label nam})]
  (cat-into
    [{:id :settings
      :label [:i {:class "zmdi zmdi-settings"}]}]
      sims
    [{:id :sims
      :label [:i {:class "zmdi zmdi-widgets"}]}])))

(defn ^:export sorted-tasks [conn]
  (let [{{:keys [sorted-tasks]} :onyx.sim/env
         {:keys [:onyx.core/catalog]} :onyx.core/job}
        (pull
          conn
          '[{:onyx.sim/env [:sorted-tasks]
             :onyx.core/job [{:onyx.core/catalog [*]}]}]
          (selected-sim conn))
        task-possible (into {} (map (juxt :onyx/name identity) catalog))
        task-choices (map task-possible sorted-tasks)]
    task-choices))

(defn ^:export any-running? [conn]
  ;; FIXME: rewrite query to use or-aggregation
  (let [sims (q '[:find ?title ?sim ?running
                    :in $
                    :where
                    [?sim :onyx/name ?sim-name]
                  [?sim :onyx.sim/title ?title]
                  [?sim :onyx/type :onyx.sim/sim]
                  [?sim :onyx.sim/running? ?running]] conn)
        any-running? (transduce
                       (map (fn [[_ _ r]]
                              r))
                       #(or %1 %2)
                       false
                       sims)]
    any-running?))

(defn env-style [conn]
  [flui/v-box
   :children
   [[control/field-label conn :onyx.sim/env-display-style]
    [control/radio-choice conn :onyx.sim/env-display-style 0]
    [control/toggle-checkbox conn :onyx.sim/render-segments?]
    [control/radio-choice conn :onyx.sim/env-display-style 1]
    [control/toggle-checkbox conn :onyx.sim/only-summary?]
    ]])

(defn action-box [conn]
  [flui/h-box
   :gap ".5ch"
   :children
   [[control/action-button conn :onyx.api/tick]
    [control/action-button conn :onyx.api/step]
    [control/action-button conn :onyx.api/drain]
    [control/toggle-button conn :onyx.sim/play]]])

(defn sim-view [conn]
  [flui/v-box
   :children
   [[control/when-show?
     [control/selection-list conn :onyx.sim/hidden-tasks]]
    [control/when-show?
     [control/indicator-display conn :onyx.sim/description]]
    [action-box conn]
    [control/when-show?
     [control/indicator-label conn :onyx.sim/next-action]]

    (when (control/simple-chosen? conn :onyx.sim/env-display-style :pretty-env)
      [pretty-env conn])
    (when (control/simple-chosen? conn :onyx.sim/env-display-style :raw-env)
      [raw-env conn])
    ]])

(defn manage-sims [conn]
  (let [sims (pull-q '[*]
               '[:find ?sim
                 :in $
                 :where
                 [?sim :onyx/type :onyx.sim/sim]
                 ] conn)]
  (flui/v-box
    :children
    (cat-into
      [(flui/title
         :label "Simulator Management"
         :level :level1)]
      (for [{:keys [:onyx.sim/title :onyx.sim/description]} sims]
        (flui/v-box
          :gap "1ch"
          :children
          [(flui/title
             :level :level2
             :label title)
           (flui/p description)]))
      [(flui/p "TODO: + Simulator")]))))

(defn settings [conn]
  (flui/v-box
    :children
    [[flui/title
         :label "Settings"
         :level :level1]
     [control/toggle-checkbox conn :onyx.sim/hidden?]
     [control/toggle-checkbox conn :onyx.sim/description?]
     [control/toggle-checkbox conn :onyx.sim/next-action?]
     [env-style conn]]))

(defmulti display-selected (fn [_ selection]
                             selection))

(defmethod display-selected
  :onyx.sim/selected-env
  [conn _]
  [sim-view conn])

(defmethod display-selected
  :settings
  [conn _]
  [settings conn])

(defmethod display-selected
  :sims
  [conn _]
  [manage-sims conn])

(defn content-view [conn]
  (let [view (selected-view conn)]
    ;; ???: bottom gap for scrolling
    [flui/box
     :class "onyx-sim"
     :child
     [display-selected conn (selected-view conn)]]))

(defn sim-selector [conn]
#?(:cljs
      [flui/v-box
        :children
        [[flui/gap :size ".25rem"]
         [flui/h-box
          :style {:margin-left "auto"
                  :margin-right "auto"}
           :align :center
           :gap "1ch"
           :children
           [[control/active-logo conn :onyx.sim/logo]
            [control/nav-bar conn :onyx.sim/nav]]]
         [flui/gap :size ".25rem"]
         [content-view conn]
         ]]
:clj
[:div "Standard HTML not yet supported."]))
