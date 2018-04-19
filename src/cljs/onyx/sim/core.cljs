(ns onyx.sim.core
  (:require [taoensso.timbre :as log]
            [onyx.sim.api :as onyx]
            [onyx.sim.flui :as flui]
            [onyx.sim.control :as control]
            [onyx.sim.svg :as svg]
            [clojure.core.async :as async]
            [clojure.spec.alpha :as s]
            [onyx.sim.event :as event]
            [onyx.sim.utils :as utils :refer [cat-into]]
            [datascript.core :as d]
            [onyx.sim.dat-view-render :as dat.view]
            [onyx.sim.examples.flow-short-circuit]
            [onyx.sim.examples.hello]
            [posh.reagent :as posh]
            [reagent.ratom :as ratom]
            [reagent.core :as r :refer [atom]]))

;; TODO: import uris should be a drop-down
;; TODO: index.html -> hiccup (prevents resources conflict with downstream projects)
;; ???: Should there be some kind of asynchronous job runner that caches compiled jobs? If so how do we get async to interface with react?

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

(def ^:private onyx-batch-size 20)

(def ^:private task-colors
  {:function sim-pale-blue
   :input onyx-gray
   :output onyx-green})

(defn pull-env [conn sim-id]
  (event/subscribe-env conn sim-id))

(def ^:private default-sim
  {:onyx/type ::sim
   ::import-uris ["verbose.edn" "route.edn" "example.edn"]
   ::running? false
   ::hidden-tasks #{}})

(defn- ds->onyx [sim-job]
  (-> sim-job
      (clojure.set/rename-keys {:onyx.core/catalog         :catalog
                                :onyx.core/workflow        :workflow
                                :onyx.core/lifecycles      :lifecycles
                                :onyx.core/windows         :windows
                                :onyx.core/triggers        :triggers
                                :onyx.core/task-scheduler  :task-scheduler
                                :onyc.core/metadata        :metadata
                                :onyx.core/flow-conditions :flow-conditions})))

(defn- init-env [{:as sim :keys [onyx.core/job ::transitions]}]
  (assoc
   sim
   ::transitions
   (into [{:event :onyx.sim.api/init
           :job (ds->onyx job)}]
         transitions)))

(defn make-sim [& {:as options}]
  (->
   (into
    default-sim
    (clojure.set/rename-keys options {:job :onyx.core/job
                                      :name :onyx/name
                                      :title ::title
                                      :description ::description
                                      :transitions ::transitions}))
   init-env))

(defn- pull-q [pull-expr query conn & input]
  (map (fn [[eid]] @(posh/pull conn pull-expr eid)) @(apply posh/q query conn input)))

;; TODO: use with new dat view represent
(def ^:private new-setting-catalog
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
    :new-sim/show-sim-description? true
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
    :dat.view/value {:e/order 6} ;; TODO: option/selected for multi-cardinality
    :dat.view/alias {:dat.view/option [:new-sim.sub/settings :new-sim/env-display-option]
                     :dat.view/selected []}}
   {:onyx/name :new-sim.sub/raw?
    :dat.view/value {:e/order 7}
    :dat.view/alias {:dat.view/selected [:new-sim.sub/settings :new-sim/env-display-style]
                     :dat.view/option [:new-sim.sub/pretty-env]}}])

(def ^:private control-catalog
  '[{:control/type :indicator
     :control/name ::next-action
     :control/label "Next Action"
     :control/show? (:onyx.sim.control/control-attr ::next-action? :control/toggled?)
     :control/display (::next-action-actual)}
    {:control/type :choice
     :control/name ::nav
     :control/label "Navigation"
     :control/chosen (::selected-nav)
     :dat.view/event {:dat.view/handler :onyx.sim.event/select-view}
     :control/choices (::view-choices)}
    {:control/type :active-logo
     :control/name ::logo
     :control/label "nyx-sim (alpha)"
     :control/img "ns/onyx/onyx-logo.png"
     :control/active? (::any-running?)}
    {:control/type :indicator
     :control/name ::description
     :control/label "Description"
     :control/show? (:onyx.sim.control/control-attr ::description? :control/toggled?)
     :control/display (::description-actual)}
    {:control/type :toggle
     :control/name ::description?
     :control/label "Show Sim Description"
     :dat.view/event {:dat.view/handler :onyx.sim.event/simple-toggle
                      :dat.view/entity [:control/name ::description?]
                      :dat.view/attr :control/toggled?}
     :control/toggled? true}
    {:control/type :toggle
     :control/name ::hidden?
     :control/label "Show Task Hider"
     :dat.view/event {:dat.view/handler :onyx.sim.event/simple-toggle
                      :dat.view/entity [:control/name ::hidden?]
                      :dat.view/attr :control/toggled?}
     :control/toggled? true}
    {:control/type :toggle
     :control/name ::next-action?
     :control/label "Show Next Action"
     :dat.view/event {:dat.view/handler :onyx.sim.event/simple-toggle
                      :dat.view/entity [:control/name ::next-action?]
                      :dat.view/attr :control/toggled?}
     :control/toggled? true}
    {:control/type :choice
     :control/name ::env-display-style
     :control/label "Environment Display Style"
     :control/chosen #{:pretty-env}
     :control/choose (:onyx.sim.control/simple-choose-one ::env-display-style)
     :control/choose-event {:dat.view/handler :onyx.sim.event/simple-toggle
                            :dat.view/entity [:control/name ::hidden?]
                            :dat.view/attr :control/chosen}
     :control/choices [{:id :pretty-env
                        :e/order 0
                        :label "Pretty"
                        :e/name "Pretty"}
                       {:id :raw-env
                        :e/order 1
                        :label "Raw"
                        :e/name "Raw"}]}
    {:control/type :toggle
     :control/name ::render-segments?
     :control/label "(Render Segments)"
     :dat.view/event {:dat.view/handler :onyx.sim.event/simple-toggle
                      :dat.view/entity [:control/name ::render-segments?]
                      :dat.view/attr :control/toggled?}
     :control/disabled? (:onyx.sim.control/simple-not-chosen? ::env-display-style :pretty-env)
     :control/toggled? true}
    {:control/type :toggle
     :control/name ::only-summary?
     :control/label "(Only Summary)"
     :dat.view/event {:dat.view/handler :onyx.sim.event/simple-toggle
                      :dat.view/entity [:control/name ::only-summary?]
                      :dat.view/attr :control/toggled?}
     :control/disabled? (:onyx.sim.control/simple-not-chosen? ::env-display-style :raw-env)
     :control/toggled? true}
    {:control/type :action
     :control/name :onyx.api/tick
     :control/label "Tick"
     :dat.view/event {:dat.view/handler :onyx.sim.event/transitions
                      ::transitions [{:event :onyx.sim.api/tick}]}
     :control/disabled? (::running?)}
    {:control/type :action
     :control/name :onyx.api/step
     :control/label "Step"
     :dat.view/event {:dat.view/handler :onyx.sim.event/transitions
                      ::transitions [{:event :onyx.sim.api/step}]}
     :control/disabled? (::running?)}
    {:control/type :action
     :control/name :onyx.api/drain
     :control/label "Drain"
     :dat.view/event {:dat.view/handler :onyx.sim.event/transitions
                      ::transitions [{:event :onyx.sim.api/drain}]}
     :control/disabled? (::running?)}
    {:control/type :toggle
     :control/name ::play
     :control/label "Play"
     :control/toggle-label "Pause"
     :control/toggled? (::running?)
     :dat.view/event {:dat.view/handler :onyx.sim.event/toggle-play}}
    {:control/type :choice
     :control/name ::hidden-tasks
     :control/label "Hidden Tasks"
     :control/chosen (::hidden-tasks)
     :dat.view/event {:dat.view/handler :onyx.sim.event/hide-tasks}
     :control/choices (::sorted-tasks)
     :control/show? (:onyx.sim.control/control-attr ::hidden? :control/toggled?)
     :control/id-fn :onyx/name
     :control/label-fn (::task-labeler)}])

(def schema-idents
  [{:db/ident :onyx.core/catalog
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/many}
   {:db/ident :onyx.sim.view/options
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/many}
   {:db/ident :onyx/name
    :db/unique :db.unique/identity}
   {:db/ident :control/name
    :db/unique :db.unique/identity}
   {:db/ident :dat.view/route
    :db/unique :db.unique/identity}
   ;; TODO: move to dat.view
   {:db/ident :dat.view.rep/layout
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/many}
   {:db/ident :onyx.core/job
    :db/valueType :db.type/ref}
   {:db/ident ::selected-sim
    :db/valueType :db.type/ref}
   {:db/ident ::env
    :db/valueType :db.type/ref}])

(def ds-schema
  (into
   {}
   (map (fn [{:as id-entity :keys [db/ident]}]
          [ident id-entity]))
   schema-idents))

;;;
;;; Predicates
;;;
(defn ^:export match-any-attr? [event old-seg seg all-new attrs]
;;   (log/info "match-attrs" (contains? attrs (last (:dat.view/path seg))))
  (contains? attrs (last (:dat.view/path seg))))

(defn ^:export always [event old-seg seg all-new]
;;   (log/info "ALWAYS")
  true)

(defn ^:export match-any-spec? [event old-seg seg all-new specs]
  (some #(s/valid? % seg) specs))

;;;
;;; pull-exprs
;;;
(def ^:private toggle-pull-expr
  '[:control/label :control/toggle-label :control/toggled? :control/toggle-event])

(def ^:private choices-pull-expr
  '[:control/label :control/chosen :control/choose-event :control/choices])

;;;
;;; Simulator Examples
;;;
(def base-ui
  (into
   [{:onyx/name ::settings
     ::selected-view :settings
     ::animation-transitions [{:event :onyx.sim.api/tick}]
     ::frames-between-animation 30
     ::animating? false}]
   control-catalog))

(def sim! event/sim!)
(def unsim! event/unsim!)

(def examples
  [(make-sim
    :name ::hello-sim
    :title "Hello Sim!"
    :description (:onyx/doc onyx.sim.examples.hello/job)
    :job onyx.sim.examples.hello/job
    :transitions [{:event :onyx.sim.api/inputs
                   :inputs {:in onyx.sim.examples.hello/input-segments}}])
   (make-sim
    :name :flow-short-circuit
    :job onyx.sim.examples.flow-short-circuit/job
    :title "Flow Short Circuit"
    :description (:onyx/doc onyx.sim.examples.flow-short-circuit/job)
    :transitions [{:event :onyx.sim.api/inputs
                   :inputs {:in onyx.sim.examples.flow-short-circuit/input-segments}}])
   [:db/add [:onyx/name ::settings] ::selected-sim [:onyx/name ::hello-sim]]
   [:db/add [:onyx/name ::settings] ::selected-view ::sim-view]])

(defn- selected-sim [conn]
  (let [{::keys [selected-sim]} @(posh/pull conn '[::selected-sim] [:onyx/name ::settings])]
;;     (log/info "selected-sim is:" selected-sim)
    (:db/id selected-sim)))

;;
;; VIEWS
;;
(defn- pretty-outbox [conn {::keys [sim task-name render]}]
  (let [{:keys [tasks]} @(pull-env conn sim)
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

(defn- pretty-inbox [conn {::keys [sim task-name render]}]
  (let [{:keys [tasks]} @(pull-env conn sim)
        {::keys [import-uris]}
        @(posh/pull conn '[::import-uris
                           {::env [*]}] sim)
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
         :on-change (partial
                     event/dispatch!
                     conn
                     {:dat.view/handler :onyx.sim.event/simple-value
                      :dat.view/entity sim
                      :dat.view/attr ::import-uri})]
        [flui/button
         :label "Import Segments"
         :on-click #(event/raw-dispatch! conn {:dat.view/handler :onyx.sim.event/import-segments
                                               ::sim sim
                                               ::task-name task-name})]]]
      [render inbox]]]))

(defn- code-render [code]
  [flui/code
   :code
   code])

(defn- pretty-task-box [conn {:as seg ::keys [sim task-name]}]
  (let [env @(pull-env conn sim)
        {::keys [render]} @(posh/pull conn '[::render] sim)
        task-type (get-in env [:tasks task-name :event :onyx.core/task-map :onyx/type])
        task-doc (get-in env [:tasks task-name :event :onyx.core/task-map :onyx/doc])
        local-render (get-in env [:tasks task-name :event :onyx.core/task-map ::render])
        render-segments? (control/control-attr conn ::render-segments? :control/toggled?)
        render-fn (if render-segments?
                    (or local-render render code-render)
                    code-render)]
    [flui/v-box
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
        [flui/button
         :label "Hide"
         :on-click
         #(event/dispatch!
           conn
           {:dat.view/handler :onyx.sim.event/hide-task
            ::sim sim
            ::task-name task-name})]
        (when task-doc [flui/label :style {:color :white} :label task-doc])])
      [pretty-inbox conn
       (assoc seg
              ::render render-fn)]
      [pretty-outbox conn
       (assoc seg
              ::render render-fn)]]]))

(defn- pretty-env [conn {:as seg ::keys [sim]}]
  (let [{:as env :keys [sorted-tasks]} @(pull-env conn sim)
        {::keys [hidden-tasks]} @(posh/pull conn '[::hidden-tasks] sim)]
    [flui/v-box
     :class "onyx-env"
     :children
     (into
      []
      (for [task-name (remove (or hidden-tasks #{}) sorted-tasks)]
        ^{:key (:onyx/name task-name)}
        [pretty-task-box conn (assoc seg ::task-name task-name)]))]))

(defn- summary [conn {::keys [sim summary-fn]}]
  (let [summary-fn (or summary-fn onyx/env-summary)
        env @(pull-env conn sim)]
    [flui/code :class "onyx-panel" :code (summary-fn env)]))

(defn- raw-env [conn {:as seg ::keys [sim]}]
  (let [only-summary? (control/control-attr conn ::only-summary? :control/toggled?)]
    [flui/v-box
     :class "onyx-env"
     :children
     [(flui/title
       :label "Raw Environment"
       :level :level3)
      [summary conn (if only-summary?
                      seg
                      (assoc seg ::summary-fn identity))]]]))

;;; control-fns
(defn- ^:export tick-action [{:as seg :keys [db/id onyx.sim.core/running?]}]
  {:control/action [[:db.fn/call onyx/tick id]]
   :control/disabled? running?})

(defn- ^:export task-labeler [conn]
  #(-> % :onyx/name pr-str))

(defn- ^:export selected-view [conn]
  (let [{::keys [selected-view]} @(posh/pull conn '[::selected-view] [:onyx/name ::settings])]
    selected-view))

(defn- ^:export selected-nav [conn]
  (let [view (selected-view conn)]
    (if (= view ::sim-view)
      (selected-sim conn)
      view)))

;; (defn ^:export toggle-play [conn]
;;   #(event/raw-dispatch conn {:onyx/type ::toggle-play
;;                              ::sim (selected-sim conn)}))

(defn- ^:export running? [conn]
  (::running? @(posh/pull conn '[::running?] (selected-sim conn))))

;; (defn ^:export tick [conn]
;;   #(event/dispatch conn {:onyx/type ::transition-env
;;                          ::transition :onyx.api/tick
;;                          ::sim (selected-sim conn)}))

;; (defn ^:export step [conn]
;;   #(event/dispatch conn {:onyx/type ::transition-env
;;                          ::transition :onyx.api/step
;;                          ::sim  (selected-sim conn)}))

;; (defn ^:export drain [conn]
;;   #(event/dispatch conn {:onyx/type ::transition-env
;;                          ::transition :onyx.api/drain
;;                          ::sim (selected-sim conn)}))

(defn- ^:export next-action-actual [conn]
  ;; TODO: feed sim instead of selected-sim
  (let [{:keys [next-action]} @(pull-env conn (selected-sim conn))]
    next-action))

(defn- ^:export description-actual [conn]
  (let [{:keys [::description]}
        @(posh/pull conn '[::description] (selected-sim conn))]
    description))

(defn- ^:export hidden-tasks [conn]
  (let [sim (selected-sim conn)]
;;     (log/info "hidden-tasks sim:" sim)
    (::hidden-tasks @(posh/pull conn '[::hidden-tasks] sim))))

(defn- ^:export view-choices [conn]
  (let [sims @(posh/q '[:find ?title ?sim
                        :in $
                        :where
                        [?sim ::title ?title]
                        [?sim :onyx/type ::sim]]
                      conn)
        sims (for [[title sim] sims]
               {:id sim
                :label title})]
    (cat-into
     [{:id :settings
       :label [:i {:class "zmdi zmdi-settings"}]}]
     sims
     [{:id :sims
       :label [:i {:class "zmdi zmdi-widgets"}]}
      {:id :debug-conn
       :label [:i {:class "zmdi zmdi-assignment"}]}])))

(defn- ^:export sorted-tasks [conn]
;;   (log/info "sorted-tasks!!!")
  (let [sim (selected-sim conn)
        {:keys [sorted-tasks]} @(pull-env conn sim)
;;          _ (log/info "sorted:" sim sorted-tasks)

        {:keys [onyx.core/job]}
        @(posh/pull
          conn
          '[{:onyx.core/job [{:onyx.core/catalog [*]}]}]
          sim)

;;         _ (log/info "catalog" job (:onyx.core/catalog job))
        task-possible (into {} (map (juxt :onyx/name identity) (:onyx.core/catalog job)))
;;         _ (log/info "task-possible" task-possible)
        task-choices (map task-possible sorted-tasks)]
    task-choices))

(defn- ^:export any-running? [conn]
  (::animating? @(posh/pull conn [::animating?] [:onyx/name ::settings])))

(defn- env-style [conn]
  [flui/v-box
   :children
   [[control/field-label conn ::env-display-style]
    [control/radio-choice conn ::env-display-style 0]
    [control/toggle-checkbox conn ::render-segments?]
    [control/radio-choice conn ::env-display-style 1]
    [control/toggle-checkbox conn ::only-summary?]]])

(defn- action-box [conn]
  [flui/h-box
   :gap ".5ch"
   :children
   [[control/action-button conn :onyx.api/tick]
    [control/action-button conn :onyx.api/step]
    [control/action-button conn :onyx.api/drain]
    [control/toggle-button conn ::play]]])

(defn sim-view [conn {:as seg ::keys [sim]}]
;;   (log/info "selected-sim" sim)
  [flui/v-box
   :children
   [[control/when-show?
     [control/selection-list conn ::hidden-tasks]]
    [control/when-show?
     [control/indicator-display conn ::description]]
    [action-box conn]
    [control/when-show?
     [control/indicator-label conn ::next-action]]

    (when (control/simple-chosen? conn ::env-display-style :pretty-env)
      [pretty-env conn seg])
    (when (control/simple-chosen? conn ::env-display-style :raw-env)
      [raw-env conn seg])]])

(defn- manage-sims [conn]
  (let [sims (pull-q '[*]
                     '[:find ?sim
                       :in $
                       :where
                       [?sim :onyx/type ::sim]]
                     conn)]
    [flui/v-box
     :children
     (cat-into
      [(flui/title
        :label "Simulator Management"
        :level :level1)]
      (for [{:keys [::title ::description]} sims]
        (flui/v-box
         :gap "1ch"
         :children
         [(flui/title
           :level :level2
           :label title)
          (flui/p description)]))
      [(flui/p "TODO: + Simulator")])]))

(defn- debug-conn [conn]
  (let [the-whole-conn (ratom/make-reaction
                        (fn []
                          @conn))]
    (fn [conn]
      [flui/code
       :code @the-whole-conn
       :pr-fn pr-str])))

(defn- settings [conn]
  [flui/v-box
   :children
   [[flui/title
     :label "Settings"
     :level :level1]
    [control/toggle-checkbox conn ::hidden?]
    [control/toggle-checkbox conn ::description?]
    [control/toggle-checkbox conn ::next-action?]
    [env-style conn]]])

;; NOTE: this should be private. Don't use this as it may disappear or change.
(defmulti display-selected (fn [_ selection]
                             selection))

(defmethod display-selected
  ::sim-view
  [conn _]
  [sim-view conn {::sim (selected-sim conn)}])

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

(defn- content-view [conn]
  (let [view (selected-view conn)]
    (log/info "selected-view" view)))
    ; [flui/box
    ;  :class "onyx-sim"
    ;  :child
    ;  [display-selected conn view]]))

(defn ^:export sim-debug [{:as sys :keys [dat.sync.db/conn onyx.sim.core/sim]}
                          {:as seg ::keys [error inputs]}]
  [flui/v-box
   :children
   [[flui/p (str "BROKEN!!! by " error)]
    [flui/p (str "Inputs" (into
                           {}
                           (map (fn [[task-name segments]]
                                  [task-name segments]))
                           inputs))]
    [sim-view conn seg]]])

(defn ^:export sim-selector
  ([conn] (sim-selector {:dat.sync.db/conn conn} nil))
  ([{:as sys :keys [dat.sync.db/conn]}
    {:as seg}]
   [flui/v-box
    :children
    [[flui/gap :size ".25rem"]
     [flui/h-box
      :style {:margin-left "auto"
              :margin-right "auto"}
      :align :center
      :gap "1ch"
      :children
      [[control/active-logo conn ::logo]]]
      ;  [control/nav-bar conn ::nav]]]
     [flui/gap :size ".25rem"]
     [content-view conn]]]))

(defn aliasify [{::keys [alias]} segment]
  (into
    segment
    (for [[aname path] alias]
      [aname 
       (if (sequential? path)
         (get-in segment path)
         (aliasify {::alias path} segment))])))

(defn ^:export nav-select [seg]
  ;;TODO: event fx
  seg)

(def nav-select-event
  {:onyx.core/catalog
   [{:onyx/name   ::event-actions
     :onyx/type   :output
     :onyx/plugin ::nav-select}]
   
   :onyx.core/workflow
   [[::event-args ::event-actions]]})

(def nav-job
  [{:onyx.core/catalog
    [{:onyx/name   ::selected-nav-q
      :onyx/type   :input
      :onyx/plugin :dat.sync.plugin/query
      :dat.sync/q  '[:find ?selected-nav
                     :where
                     [_ :onyx.sim.settings/selected-nav ?selected-nav]]}
     {:onyx/name    ::attrs-in
      :onyx/plugin  :onyx.plugin/seq
      :onyx.plugin.seq/seq [{::label-attr ::label}
                            {::id-attr    ::id}
                            {::on-change  nav-select-event}]}
     {:onyx/name      ::selected-nav-alias
      :onyx/type      :function
      :onyx/fn        ::aliasify
      :dat.view/alias {:dat.view/chosen [:?selected-nav]}}
     {:onyx/name    ::nav-choices-q
      :onyx/type    :input
      :onyx/plugin  :dat.sync.plugin/query
      :dat.sync/q   '[:find ?title ?sim
                      :where
                      [?sim ::title    ?title]
                      [?sim :oynx/type ::sim]]}
     {:onyx/name        ::nav-choices-alias
      :onyx/type        :function
      :onyx/fn          ::aliasify
      :dat.view/alias {::label [:?title]
                       ::id    [:?sim]}}
     dat.view/mount-task]

    :onyx.core/workflow
    [[::selected-nav-q     ::selected-nav-alias]
     [::selected-nav-alias ::render]
     [::nav-choices        ::nav-choices-alias]
     [::nav-choices-alias  :dat.view.render/mount]
     [::attrs-in           :dat.view.render/mount]]}])

(defn sim-selector2 [{:as sys :keys [dat.sync.db/conn]}]
  (let [segments [{:dat.view.render/type :dat.view.render/nav}
                  {:dat.view/choice {:dat.view/label "One"
                                     :dat.view/id 1}}
                  {:dat.view/choice {:dat.view/label "Two"
                                     :dat.view/id 2}}
                  {:dat.view/choice {:dat.view/label "Three"
                                     :dat.view/id 3}}
                  {:dat.view/id-attr {:dat.view/attr :dat.view/id}}
                  {:dat.view/label-attr {:dat.view/attr :dat.view/label}}
                  {:dat.view/on-change nav-select-event}
                  {:dat.view/chosen {:dat.view/id 1}}]]
    [dat.view/quick-render segments]))
