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

(def onyx-batch-size 20)

;; FIXME: move fns around
(declare with-context)
(declare map-alias)

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
                     :dat.view/option [:new-sim.sub/pretty-env]}}

   {:onyx/name :pull-attrs}
   :dat.view/layout-expr
   '{:route/index
     [{:new-sim.sub/top-bar
       [{:new-sim.sub/logo
         [:new-sim.sub/active-icon :new-sim.sub/active-icon-label]}
        {:new-sim.sub/tab-bar
         [:new-sim.sub/settings-icon
          {:new-sim.sub/sims-icon [{* [:e/name]}]}
          :new-sim.sub/config-sims-icon]}]}
      {:new-sim.sub/main-panel
       #{{:new-sim.sub/selected-sim
          [:new-sim.sub/hidden-tasks-label
           :new-sim.sub/hidden-tasks
           {:new-sim.sub/control [*]}
           {:new-sim.sub/action-bar
            [:new-sim.sub/next-action]}
           {:new-sim.sub/task [*]}]}
         {:new-sim.sub/settings
          [:new-sim.sub/settings-label
           :new-sim/show-sim-task-hider?
           :new-sim/show-sim-description?
           :new-sim/show-next-action?
           :new-sim.sub/env-display-label
           :new-sim.sub/pretty?
           :new-sim.sub/render-segments?
           :new-sim.sub/raw?
           :new-sim.sub/only-summary?]}
         {:new-sim.sub/sim-manager
          [:new-sim.sub/sim-manager-title
           {:new-sim.sub/manage-sim
            [{* [:e/name :e/description]}]}
           :new-sim.sub/add-sim]}}}]}])

(defn pull-clean-env [conn sim-id]
  (:onyx.sim/clean-env (pull conn '[:onyx.sim/clean-env] sim-id)))

(defn render-segment [conn sim-id seg]
  (log/info "rendering seg" seg)
  (let [env (pull-clean-env conn sim-id)
        _ (log/info "env" (keys env))
         drained-env (-> env
                         (onyx/new-segment :dat.view/render seg)
                         onyx/drain)
         ]
    (-> drained-env
        :tasks
        :dat.view/mount2
        :outputs
        first
        :dat.view/component)))

(defn render-segments->debug-sim [db parent-sim-id segs child-name]
  (let [parent-sim (d/entity db parent-sim-id)]
    (log/info "parent job" (:onyx.core/job parent-sim))
  [(into
     default-sim
     {:onyx.sim/title child-name
      :onyx/type :onyx.sim/sim
      :onyx.core/job (get-in parent-sim [:onyx.core/job :db/id])
      :onyx.sim/env (reduce #(onyx/new-segment %1 :dat.view/render %2) (:onyx.sim/clean-env parent-sim) segs)
      :onyx.sim/clean-env (:onyx.sim/clean-env parent-sim)})]))

(defn dat-view-box-actual [{:keys [dat.sync/conn]}
                           {:as seg :keys [dat.view/direction dat.view/layout dat.view/style]}]
  (log/info "dat-view-box" layout)
  (let [sim-id [:onyx/name :verbose-sim]
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

(defn ^:export dat-view-box [sys seg]
  (assoc
    seg
    :dat.view/component
    [dat-view-box-actual sys seg]))

(defn ^:export dat-view-label [sys {:as seg :keys [dat.view/label]}]
  (assoc
    seg
    :dat.view/component
    [flui/label :label label]))

(defn ^:export dat-view-checkbox [{:as sys :keys [dat.view/dispatch!]}
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

(defn ^:export dat-view-text-input [{:as sys :keys [dat.view/dispatch!]} {:as seg :keys [dat.view/label dat.view/event]}]
  (assoc
    seg
    :dat.view/component
    [flui/input-text
     :model label
     :on-change (partial dispatch! (with-context event seg))]))

(defn ^:export dat-view-default [seg]
  (assoc
    seg
    :dat.view/component
    [flui/p (str "Unknown representation:" seg)]))

(defn ^:export dat-view-router [{:keys [dat.sync/conn]} {:as seg :keys [dat.view/route db/id]}]
  (log/info "Routing (or " id route ")")
  (into
    seg
    (pull conn '[*] (or id [:dat.view/route route]))))

(defn ^:export dat-view-pull [{:as sys :keys [dat.sync/conn]}
                              {:as seg :keys [dat.view/pull-expr
                                              dat.view/entity
                                              dat.view/alias]}]
  (map-alias
    alias
    (into
      seg
      (when pull-expr
        (pull conn pull-expr entity)))))

(defn parse-find-vars [q-expr]
  ;; TODO: implement. for now a hardcoded value for testing
  '[:?todo])

(defn ^:export dat-view-q [{:as sys :keys [dat.sync/conn]}
                           {:as seg :keys [dat.view/q-expr
                                           dat.view/inputs
                                           dat.view/layout-alias
                                           dat.view/layout-value]}]
  (let [find-vars (parse-find-vars q-expr)
        relation (when q-expr
                   (apply q q-expr conn inputs))]
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

(defn with-subscriptions [system {:as seg :keys [dat.view/subscription]}]
  (into
    seg
    ;; TODO: run subscription through onyx if it exists
    nil
    ))

(defn process-tab [system tab]
  (with-subscriptions system tab))

(defn expand-tab-sets [system]
  (fn [step]
    (fn
      ([] (step))
      ([acc] (step acc))
      ([in acc]
       (if (= (:dat.view/represent in) :dat.view.control/tab-set)
         (transduce (map #(process-tab system %)) step acc in)
         (step acc (process-tab system in)))))))

(defn process-tabs [system tabs]
  (into
    []
    (expand-tab-sets system)
    (sort-by :e/order tabs)))

(defn ^:export robust-tab-bar [{:as system :keys [dat.view/dispatch!]} {:keys [dat.view/tabs dat.view/event]}]
  (let [tabs (process-tabs system tabs)]
    {:dat.view/component
     [flui/horizontal-bar-tabs
      :tabs tabs
      :label-fn :dat.view.control/tab-label
      :id-fn :dat.view.control/tab-id
      :on-change (partial dispatch! event)
      ;; TODO: figure out what horizontal-bar-tabs returns for the event value. Then create a handler that works like ::simple-value that can handle this input type.
      ]}))

(defn ^:export robust-box [system {:keys [dat.view/direction dat.view/style dat.view/layout]}]
  [(case direction
     :horizontal flui/h-box
     flui/v-box)
   :style style
   :children
   (for [item layout]
     ;; ???: process item through onyx
     (:dat.view/component item))])

(defn ^:export robust-modal-box [system {:keys [dat.view/mode dat.view/layout]}]
  [flui/box
   :child
   ;; ???: process item through onyx
   (:dat.view/component (first (filter #(= mode (:dat.view/mode %)) layout)))])

(def new-sim-workflow
  [[:route/settings :new-sim.sub/settings]
   [:new-sim.sub/settings :dat.view/pull-form]
   [:new.sim.sub/settings-label :dat.view.control/title]
   [:new.sim.sub/env-diplay-label :dat.view.control/title]
   [:new-sim.sub/settings :new-sim.sub/pretty-env]
   [:new-sim.sub/settings :new-sim.sub/raw-env]
   [:new-sim.sub/pretty-env :new-sim.sub/pretty?]
   [:new-sim.sub/raw-env :new-sim.sub/raw?]
   [:new-sim.sub/pretty? :radio-choice]
   [:new-sim.sub/raw? :radio-choice]
   ])


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
     :control/label-fn :onyx/name}])




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

(defn ^:export with-context [{:as event :keys [dat.view/alias]} seg]
  (into
    event
    (map-alias alias seg)))

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

(defn ^:export split-attrs2 [{:keys []} {:as seg :keys []}]

  )

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

(def ^:export dat-view-lifecycle
  {:lifecycle/before-task-start (context-injecter conn-context dispatch-context)})

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

(defn ^:export represent? [event old-seg seg all-new represent]
  (log/info "represent?" represent (= (:dat.view/represent seg) represent))
  (= (:dat.view/represent seg) represent))

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

(def render-sim
  (into
    default-sim
    {:onyx/name :render-example-env
     :onyx.sim/title "Render Example"
     :onyx.sim/description "Sandbox to create new dat.view components"
     :onyx.core/job the-job}))


(def todos-query
  '[:find ?todo
    :in $
    :where
    [?todo :e/type :e.type/Todo]])

(def verbose-hello-world
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

(def verbose-catalog
  [{:onyx/name :dat.view/render
    :onyx/type :input
    :onyx/batch-size onyx-batch-size}
   {:onyx/name :dat.view.subscribe/route
    :onyx/type :function
    :onyx/fn ::dat-view-router
    :onyx/batch-size onyx-batch-size}
   {:onyx/name :dat.view.represent/box
    :onyx/type :function
    :onyx/fn ::dat-view-box
    :onyx/batch-size onyx-batch-size}
   {:onyx/name :dat.view.represent/label
    :onyx/type :function
    :onyx/fn ::dat-view-label
    :onyx/batch-size onyx-batch-size}
   {:onyx/name :dat.view.represent/checkbox
    :onyx/type :function
    :onyx/fn ::dat-view-checkbox
    :onyx/batch-size onyx-batch-size}
   {:onyx/name :dat.view.subscribe/pull
    :onyx/type :function
    :onyx/fn ::dat-view-pull
    :onyx/batch-size onyx-batch-size}
   {:onyx/name :dat.view.subscribe/q
    :onyx/type :function
    :onyx/fn ::dat-view-q
    :onyx/batch-size onyx-batch-size}
   {:onyx/name :dat.view.represent/text-input
    :onyx/type :function
    :onyx/fn ::dat-view-text-input
    :onyx/batch-size onyx-batch-size}
   {:onyx/name :dat.view.represent/default
    :onyx/type :function
    :onyx/fn ::dat-view-default
    :onyx/batch-size onyx-batch-size}
   {:onyx/name :dat.view/mount2
    :onyx/type :output
    :onyx.sim/render sim-render
    :onyt/batch-size onyx-batch-size}])

(def verbose-sim
  (into
    default-sim
    {:onyx/type :onyx.sim/sim
     :onyx/name :verbose-sim
     :onyx.sim/title "Verbose Method Sim"
     :onyx.sim/description "Verbose Layout Method"
     :onyx.core/job {:onyx/type :onyx.core/job
                     :onyx.core/catalog verbose-catalog

                     :onyx.core/lifecycles
                     [{:lifecycle/task :dat.view.subscribe/route
                       :lifecycle/calls ::dat-view-lifecycle
                       :onyx.sim/system :onyx.sim.system/system}
                      {:lifecycle/task :dat.view.subscribe/pull
                       :lifecycle/calls ::dat-view-lifecycle
                       :onyx.sim/system :onyx.sim.system/system}
                      {:lifecycle/task :dat.view.subscribe/q
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
                      [:dat.view.subscribe/route :dat.view.subscribe/q]
                      [:dat.view.subscribe/q :dat.view.subscribe/pull]

                      [:dat.view.subscribe/pull :dat.view.represent/default]
                      [:dat.view.subscribe/pull :dat.view.represent/label]
                      [:dat.view.subscribe/pull :dat.view.represent/checkbox]
                      [:dat.view.subscribe/pull :dat.view.represent/text-input]
                      [:dat.view.subscribe/pull :dat.view.represent/box]

                      [:dat.view.represent/default :dat.view/mount2]
                      [:dat.view.represent/label :dat.view/mount2]
                      [:dat.view.represent/checkbox :dat.view/mount2]
                      [:dat.view.represent/text-input :dat.view/mount2]
                      [:dat.view.represent/box :dat.view/mount2]]

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
                       :flow/predicate ::always
                       :flow/short-circuit? true}]}}))

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

(def base-ui
  (into
    (into control-catalog verbose-hello-world)
    [(init-job hello-sim)
     (init-job render-sim)
     (init-job verbose-sim)
     (make-sim
       :name :flow-short-circuit
       :job onyx.sim.examples.flow-short-circuit/job
       :title "Flow Short Circuit"
       :description (:onyx/doc onyx.sim.examples.flow-short-circuit/job))
     {:onyx/name :onyx.sim/settings
      :onyx.sim/selected-view :onyx.sim/selected-env
      :onyx.sim/selected-env [:onyx/name :verbose-sim]}]))

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
      :label [:i {:class "zmdi zmdi-assignment"}]}])))

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
  (let [ssim-id (q '[:find ?sim .
                         :in $
                         :where
                         [?sim :onyx/type :onyx.sim/sim]
                         [?sim :onyx.sim/title "Spawned Sim"]] conn)
        spawned-sim (pull conn [:onyx.sim/title :onyx.core/job] ssim-id)]
    [flui/v-box
     :children
     [[flui/p
       (str spawned-sim)]
      [flui/p
       (str spawned-sim)]]]))

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
