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

(def onyx-batch-size 20)

(def task-colors
  {:function sim-pale-blue
   :input onyx-gray
   :output onyx-green})

(def default-sim
  {:onyx/type :onyx.sim/sim
   :onyx.sim/import-uris ["verbose.edn" "route.edn" "example.edn"]
   :onyx.sim/speed 1.0
   :onyx.sim/running? false
   :onyx.sim/hidden-tasks #{}})

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

(defn init-job [{:as sim :keys [onyx.core/job]}]
  (let [env (onyx/init (ds->onyx job))]
    (assoc
      sim
      :onyx.sim/env env
      :onyx.sim/clean-env env)))

(defn make-sim [& {:as options}]
  (init-job
    (into
      default-sim
      (clojure.set/rename-keys options {:job :onyx.core/job
                                        :name :onyx/name
                                        :title :onyx.sim/title
                                        :description :onyx.sim/description}))))
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

;; ***TODO: use with new dat view represent
(def new-setting-catalog
  [{:db/valueType :db.type/ref
    :e/type :e.type/type
    :db/ident :new-sim/env-display-style
    :db/cardinality :db.cardinality/one
    :e.type/attrs [:e/name :onyx/name :e/order :new-sim/env-display-option]}
   {:db/valueType :db.type/ref
    :e/type :e.type/type
    :db/ident :new-sim/env-display-option
    :db/cardinality :db.cardinality/many
    :e.type/attrs [:e/name :onyx/name]}
   {:db/ident :new-sim/show-task-hider?
    :db/valueType :db.type/boolean
    :db/cardinality :db.cardinality/one
    :db/doc "Show the task hider for onyx sim?"
    :e/name "Show Task Hider"}
   {:db/ident :new-sim/show-sim-description?
    :db/valueType :db.type/boolean
    :db/cardinality :db.cardinality/one
    :db/doc "Show the description for onyx sim?"
    :e/name "Show Sim Description"}
   {:db/ident :new-sim/show-next-action?
    :db/valueType :db.type/boolean
    :db/cardinality :db.cardinality/one
    :db/doc "Show the next action for onyx sim?"
    :e/name "Show Next Action"}
   {:e/type :e.type/type
    :db/ident :new-sim
    :e.type/attrs [:e/name :e/description :e/order :onyx/name :new-sim/env-display-style :new-sim/env-display-option :new-sim/show-task-hider? :new-sim/show-next-action? :new-sim/show-sim-description?]}
   {:e/name "(Render Segments)"
    :e/type :new-sim/env-display-option
    :onyx/name :new-sim/raw-env}
   {:e/name "(Only Summary)"
    :e/type :new-sim/env-display-option
    :onyx/name :new-sim/only-summary}
   {:e/name "Pretty"
    :e/type :new-sim/env-display-style
    :onyx/name :new-sim/pretty-env
    :e/order 0
    :new-sim/env-display-options [[:onyx/name :new-sim/render-segments]]}
   {:e/name "Raw"
    :e/type :new-sim/env-display-style
    :onyx/name :new-sim/raw-env
    :e/order 1
    :new-sim/env-display-options [[:onyx/name :new-sim/only-summary]]}
   {:onyx/name :new-sim
    :e/name "New Settings Sim"
    :e/description "Sim using the new settings style that integrates more closely with dat.view"
    :e/order 0
    :new-sim/env-display-style [:onyx/name :new-sim/pretty-env]
    :new-sim/env-display-options [[:onyx/name :new-sim/render-segments]]
    :new-sim/show-task-hider? true
    :new-sim/show-sim-description? false
    :new-sim/show-next-action? true}
   {:onyx/name :new-sim.sub/settings
    :onyx/type :function
    :dat.view/entity [:onyx/name :new-sim]
    :dat.view/pull-expr [:e/name :e/description :e/order {:new-sim/env-display-style [:e/name]} {:new-sim/env-display-option [:e/name]} :new-sim/show-task-hider? :new-sim/show-next-action? :new-sim/show-sim-description?]}

   {:onyx/name :new-sim.sub/settings-label
    :dat.view/value {:e/name "Settings"}}
   {:onyx/name :new-sim.sub/env-display-label
    :dat.view/value {:e/name "Environment Display Style"}}
   {:onyx.name :new-sim.sub/pretty-env
    :dat.view/entity [:onyx/name :new-sim/pretty-env]
    :dat.view/pull-expr '[:e/name]}
   {:onyx.name :new-sim.sub/raw-env
    :dat.view/entity [:onyx/name :new-sim/raw-env]
    :dat.view/pull-expr '[:e/name]}
   {:onyx/name :new-sim.sub/pretty?
    :dat.view/value {:e/order 5}
    :dat.view/alias {:dat.view/selected [:new-sim.sub/settings :new-sim/env-display-style]
                     :dat.view/option [:new-sim.sub/pretty-env]}}
   {:onyx/name :new-sim.sub/render-segments?
    :dat.view/value {:e/order 6} ;; ***TODO: option/selected for multi-cardinality
    :dat.view/alias {:dat.view/option [:new-sim.sub/settings :new-sim/env-display-option]
                     :dat.view/selected []}}
   {:onyx/name :new-sim.sub/raw?
    :dat.view/value {:e/order 7}
    :dat.view/alias {:dat.view/selected [:new-sim.sub/settings :new-sim/env-display-style]
                     :dat.view/option [:new-sim.sub/pretty-env]}}])

(defn pull-clean-env [conn sim-id]
  (:onyx.sim/clean-env (pull conn '[:onyx.sim/clean-env] sim-id)))

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
                        :label "Pretty"
                        :e/name "Pretty"

                        }
                       {:id :raw-env
                        :e/order 1
                        :label "Raw"
                        :e/name "Raw"}]}
    {:control/type :toggle
     :control/name :onyx.sim/render-segments?
     :control/label "(Render Segments)"
     :control/toggle (:onyx.sim.control/simple-toggle :onyx.sim/render-segments?)
     :control/disabled? (:onyx.sim.control/simple-not-chosen? :onyx.sim/env-display-style :pretty-env)
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
     :control/label-fn (::task-labeler)}])

(def ds-schema
  {:onyx.core/catalog {:db/type :db.type/ref
                       :db/cardinality :db.cardinality/many}
   :onyx.sim.view/options {:db/type :db.type/ref
                           :db/cardinality :db.cardinality/many}
   :onyx/name {:db/unique :db.unique/identity}
   :control/name {:db/unique :db.unique/identity}
   :dat.view/route {:db/unique :db.unique/identity}
   :dat.view/layout {:db.type :db.type/ref
                     :db/cardinality :db.cardinality/many}
   :onyx.core/job {:db/type :db.type/ref}
   :onyx.sim/selected-env {:db/type :db.type/ref}
   :onyx.sim/env {:db/type :db.type/ref}})

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

(defn context-injecter [& fns]
  (fn [event lifecycle]
    {:onyx.core/params
     [(transduce
       (map #(% event lifecycle))
       merge
       {}
       fns)]}))

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
;;; Simulator Examples
;;;
(defn ^:export hello [seg]
  (assoc seg :hello-msg "Hello, world!"))

(def hello-sim
  (make-sim
    :name ::hello-sim
    :title "Hello Sim!"
    :description "Simulation Example."
    :job {:onyx/type :onyx.core/job
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
          }))

(def base-ui
  (into
    control-catalog
    [hello-sim
     (make-sim
       :name :flow-short-circuit
       :job onyx.sim.examples.flow-short-circuit/job
       :title "Flow Short Circuit"
       :description (:onyx/doc onyx.sim.examples.flow-short-circuit/job))
     {:onyx/name :onyx.sim/settings
      :onyx.sim/selected-view :onyx.sim/selected-env
      :onyx.sim/selected-env [:onyx/name ::hello-sim]}]))

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
(defn ^:export tick-action [{:as seg :keys [db/id onyx.sim/running?]}]
  {:control/action [[:db.fn/call onyx/tick id]]
   :control/disabled? running?})

(defn ^:export task-labeler [conn]
  #(-> % :onyx/name pr-str))

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
      :label [:i {:class "zmdi zmdi-widgets"}]}
     {:id :debug-conn
      :label [:i {:class "zmdi zmdi-assignment"}]}
     ])))

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

(defn debug-conn [conn]
  [flui/p
   (pr-str @conn)])

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
  :debug-conn
  [conn _]
  [debug-conn conn])

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

(defn sim-debug [env]
  [flui/p
   (str "BROKEN!!!" (onyx/env-summary env))])

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
