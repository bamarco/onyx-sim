(ns onyx.sim.core
  (:require [taoensso.timbre :as log]
            [onyx.sim.api :as onyx]
            [onyx.sim.flui :as flui]
            [onyx.sim.control :as control]
            [onyx.sim.svg :as svg]
            [onyx.sim.event :as event :refer [dispatch raw-dispatch]]
            [onyx.sim.utils :as utils :refer [cat-into]]
            [datascript.core :as d]
            #?(:cljs [posh.reagent :as posh])
            #?(:cljs [reagent.core :as r :refer [atom]])))
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
     :control/toggled? false}
    {:control/type :toggle
     :control/name :onyx.sim/hidden?
     :control/label "Show Task Hider"
     :control/toggle (:onyx.sim.control/simple-toggle :onyx.sim/hidden?)
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
     :control/choices [{:id :pretty-env
                        :label "Pretty"}
                       {:id :raw-env
                        :label "Raw"}]}
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

(def task-colors
  {:function sim-pale-blue
   :input onyx-gray
   :output onyx-green})

(def default-sim
  {:onyx/type :onyx.sim/sim
   :onyx.sim/import-uris ["example.edn"]
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
      (clojure.set/rename-keys {:onyx.core/catalog :catalog
                                :onyx.core/workflow :workflow
                                :onyx.core/lifecycles :lifecycles
                                :onyx.core/flow-conditions :flow-conditions})))

(def base-ui
  (into
    control-catalog
    [(into
       default-sim
       {:onyx/name :hello-env
        :onyx.sim/title "Hello Sim!"
        :onyx.sim/description "Simulation Example."
        :onyx.core/job {:onyx/type :onyx.core/job}})
     {:onyx/name :onyx.sim/settings
      :onyx.sim/selected-view :onyx.sim/selected-env
      :onyx.sim/selected-env [:onyx/name :hello-env]}]))

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
        {:keys [:onyx.sim/import-uri]
         {tasks :tasks} :onyx.sim/env}
        (pull conn '[:onyx.sim/import-uri
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
         :model (str import-uri)
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
        {:keys [:onyx.sim/render]
         {tasks :tasks} :onyx.sim/env}
        (pull conn '[:onyx.sim/render {:onyx.sim/env [*]}] sim-id)
        task-type (get-in tasks [task-name :event :onyx.core/task-map :onyx/type])
        local-render (get-in tasks [task-name :onyx.sim/render])
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
  [sim-view conn]
  )

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
