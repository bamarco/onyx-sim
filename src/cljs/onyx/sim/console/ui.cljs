(ns onyx.sim.console.ui
  (:require
    [re-com.core :as re-com]
    [taoensso.timbre :as log]
    [clojure.spec.alpha :as s]
    [posh.reagent :as posh]
    [onyx.sim.api :as api]
    [onyx.sim.kb :as kb]
    [onyx.sim.components.dispatcher :as dispatcher]
    [onyx.sim.console.subscription :as sub]
    [datascript.core :as d]
    [onyx.sim.kb :as kb]
    [reagent.ratom :as ratom]
    [clojure.core.async :as async :refer [go go-loop >! <! alts!]]
    [onyx.sim.utils :as utils :refer [forv gen-uuid third deref-or-value ppr-str mapply]]))

(def schema-idents
  [{:db/ident ::name
    :db/unique :db.unique/identity}])

(def base-ui    
  [{::name ::settings
    ::selected-nav ::settings
    ::task-hider? true
    ::description? false
    ::next-action? true
    ::env-style ::pretty-env ;; ::raw-env
    ::render-segments? true
    ::only-summary? true
    ::animation-transitions [{:event :onyx.sim.api/tick}]
    ::frames-between-animation 30
    ::animating? false}])

;;;
;;; Colors
;;;
(def onyx-green "#43d16b")
(def onyx-gray "#354d56")
(def sim-blue "#6a8aac")
(def sim-pale-blue "#9ee3ff")
(def sim-gold "#c0b283")
(def sim-dark-tan "#A07F60")
(def sim-light-tan "#D7CEC7")

(def ^:private task-colors
  {:function sim-pale-blue
   :input onyx-gray
   :output onyx-green})

(defn subscribe [model q-expr-or-fn & inputs]
  ; (log/info "subscribing" q-expr-or-fn)
  (if (fn? q-expr-or-fn)
    (ratom/make-reaction #(apply q-expr-or-fn (:knowbase model) inputs))
    (apply kb/sub (:knowbase model) q-expr-or-fn inputs)))

(def listen (comp deref subscribe))

(defn dispatch! [model & event]
  (apply dispatcher/dispatch! (:dispatcher model) event))

;;;
;;; Re-com Style Components
;;;
(defn code
  "Eventually a pretty lookin code block with syntax highlighting."
  [& {:as args :keys [code child pr-fn] cl :class}]
  ;; TODO: word-wrap and line numbers
  ;; TODO: syntax highlighting
  (let [args (-> args
                (dissoc :code :pr-fn)
                (assoc :class (str "rc-code " cl)))
        code ((or pr-fn ppr-str) code)]
    (assert (not child) (str "Code should not have a :child element. Got " child))
    (mapply re-com/box :child [:code [:pre code]] args)))

(defn active-logo [& {:keys [img active? label]}]
  [re-com/h-box
    :class "active-logo"
    :children
    [[re-com/box :child [:img
                          {:class (str "active-logo-img" (when (deref-or-value active?) " spinning"))
                           :src (deref-or-value img)}]]
     [re-com/label :label (deref-or-value label)]]])

(defn toggle-checkbox [& {:keys [disabled? label toggle-label toggled? on-change]}]
  [re-com/checkbox
   :model toggled?
   :disabled? disabled?
   :label (if toggled? (or toggle-label label) label)
   :on-change on-change])

;;;
;;; Irregular Components
;;;
(def none [:span])

(defn warn
  ""
  [& args]
  [re-com/label :label (apply ppr-str args)])

(defn field-label [label]
  [re-com/label
    :class "field-label"
    :label label])

;;;
;;; Core Components
;;;
(defn logo [model]
  (let [animating? (listen model sub/?animating)]
    ; (log/info "animating?" animating?)
    [active-logo
      :img "ns/onyx/onyx-logo.png"
      :active? animating?
      :label "nyx-sim (alpha)"]))

(defn- env-style [model]
  (let [{::keys [only-summary? render-segments? env-style]} (listen model sub/?settings)
        pretty? (= env-style ::pretty-env)
        raw?    (= env-style ::raw-env)]
    [re-com/v-box
      :children
      [
        [field-label "Environment Display Style"]
        [re-com/radio-button
          :label "Pretty"
          :model env-style
          :value ::pretty-env
          :on-change #(dispatch! model :onyx.sim.console.event/eav :e [::name ::settings] :a ::env-style :v ::pretty-env)]
        [re-com/checkbox
          :label "(Render Segments)"
          :model render-segments?
          :disabled? raw?
          :on-change #(dispatch! model :onyx.sim.console.event/eav :e [::name ::settings] :a ::render-segments? :v (not render-segments?))]
        [re-com/radio-button
          :label "Raw"
          :model env-style
          :value ::raw-env
          :on-change #(dispatch! model :onyx.sim.console.event/eav :e [::name ::settings] :a ::env-style :v ::raw-env)]
        [re-com/checkbox
          :label "(Only Summary)"
          :model only-summary?
          :disabled? pretty?
          :on-change #(dispatch! model :e [::name ::settings] :a ::only-summary? :v (not only-summary?))]]]))

(defn- settings [model]
  (let [{::keys [task-hider? description? next-action?]} (listen model sub/?settings)]
    [re-com/v-box
      :children
      [
        [re-com/title
          :label "Settings"
          :level :level1]
        [re-com/checkbox 
          :model task-hider?
          :label "Show Task Hider"
          :on-change #(dispatch! model :onyx.sim.console.event/eav :e [::name ::setttings] :a ::task-hider? :v (not task-hider?))]
        [re-com/checkbox
          :model description?
          :label "Show Job Description"
          :on-change #(dispatch! model :onyx.sim.console.event/eav :e [::name ::settings] :a ::description? :v (not description?))]
        [re-com/checkbox 
          :model next-action?
          :label "Show Next Action"
          :on-change #(dispatch! model :onyx.sim.console.event/eav :e [::name ::settings] :a ::next-action? :v (not next-action?))]
        [env-style model]]]))

(defn- hidden-tasks [model job-id]
  (let [choices (or (listen model sub/sorted-task-labels job-id) [])
        {::keys [hidden-tasks]} (listen model sub/?hidden-tasks)
        hidden-tasks (or hidden-tasks #{})]
    [re-com/v-box
      :children
      [
        [field-label "Hidden Tasks"]
        [re-com/selection-list
          :choices choices
          :model hidden-tasks
          :on-change #(dispatch! model :onyx.sim.console.event/eav :e [:onyx/job-id job-id] :a ::hidden-tasks :v %)]]]))

(defn- default-render [segs]
  [code :code segs])

(defn- pretty-outbox [model job-id task-name & {:keys [render]}]
  (let [tasks (listen model sub/env-in [job-id :tasks])
        outputs (get-in tasks [task-name :outputs])]
    ;; ???: dump segments button
    (if-not outputs none
      [re-com/v-box
       :class "onyx-outbox"
       :children
       [[re-com/title
         :label "Outbox"
         :level :level3]
        [render outputs]]])))

(defn- pretty-inbox [model job-id task-name & {:keys [render]}]
  (let [tasks (listen model sub/env-in [job-id :tasks])
        inbox (get-in tasks [task-name :inbox])]
    [re-com/v-box
     :class "onyx-inbox"
     :children
     [[re-com/title
         :label "Inbox"
         :level :level3]
      [render inbox]]]))

(defn- pretty-task-box [model job-id task-name]
  (let [env (listen model sub/env-in [job-id])
        {::keys [render]} (listen model sub/?job-expr ::expr [::render])
        task-type (get-in env [:tasks task-name :event :onyx.core/task-map :onyx/type])
        task-doc (get-in env [:tasks task-name :event :onyx.core/task-map :onyx/doc])
        local-render (get-in env [:tasks task-name :event :onyx.core/task-map ::render])
        {::keys [render-segments?]} (listen model sub/?settings)
        render-fn (if render-segments?
                    (or local-render render default-render)
                    default-render)]
    [re-com/v-box
      :class "onyx-task onyx-panel"
      :gap ".25rem"
      :children
      [
        [re-com/h-box
          :gap ".5ch"
          :align :center
          :style {:background-color (get task-colors task-type)
                  :border-radius :5px}
          :children
          [
            [re-com/gap :size ".5ch"]
            [re-com/title
              :label task-name
              :level :level2]
            [re-com/button
              :label "Hide"
              :on-click #(dispatch! model :onyx.sim.console.event/hide-task :selected-job job-id :task-name task-name)]]]
        (when task-doc
          [re-com/label :style {:color :white} :label task-doc])
        [pretty-inbox model job-id task-name :render render-fn]
        [pretty-outbox model job-id task-name :render render-fn]]]))

(defn- pretty-env [model job-id]
  (let [sorted-tasks (listen model sub/env-in [job-id :sorted-tasks])
        {::keys [hidden-tasks]} (listen model sub/?hidden-tasks)]
    [re-com/v-box
     :class "onyx-env"
     :children
     (into
      []
      (for [task-name (remove (or hidden-tasks #{}) sorted-tasks)]
        ^{:key task-name}
        [pretty-task-box model task-name]))]))

(defn- summary [model job-id & {:keys [summary-fn]}]
  (let [summary-fn (or summary-fn api/env-summary)
        env (listen model sub/env-in [job-id])]
    [code :class "onyx-panel" :code (summary-fn env)]))

(defn- raw-env [model job-id]
  (let [{::keys [only-summary?]} (listen model sub/?settings)]
    [re-com/v-box
     :class "onyx-env"
     :children
     [[re-com/title
       :label "Raw Environment"
       :level :level3]
      [summary model job-id :summary-fn (when-not only-summary? identity)]]]))

(defn- next-action [model job-id]
  (let [next-action (listen model sub/env-in [job-id])]
    [re-com/h-box
      :gap "1ch"
      :children
      [
        [re-com/label
          :class "field-label"
          :label "Next Action"]
        [re-com/label
          :label (str next-action)]]]))

(defn- description [model job-id]
  (let [{:onyx/keys [doc]} (listen model sub/?job-expr ::expr [:onyx/doc] ::job-id job-id)]
    [re-com/p doc]))

(defn- action-bar [model]
  ;; FIXME: running?
  (let [running? (listen model sub/?animating)]
    [re-com/h-box
      :gap ".5ch"
      :children
      [
        [re-com/button
          :label "Tick"
          :disabled? running?
          :on-click #()]
        [re-com/button
          :label "Step"
          :disabled? running?
          :on-click #()]
        [re-com/button
          :label "Drain"
          :disabled? running?
          :on-click #()]
        [re-com/button
          :label (if running? "Stop" "Play")
          :on-click #()]]]))

(defn job-view [model job-id]
  (let [{::keys [env-style next-action? task-hider? description?]} (listen model sub/?settings)]
    [re-com/v-box
      :children
      [
        (when task-hider?
          [hidden-tasks model job-id])
        (when description?
          [description model job-id])
        (when next-action?
          [next-action model job-id])
        [action-bar model]
        (case env-style
          ::pretty-env [pretty-env model job-id]
          ::raw-env    [raw-env model job-id]
          [warn "Unknown environment style" env-style])]]))

(def to-uuid cljs.reader/read-string)

(defn submit-job-button [model job-catalog-id]
  (let [{:onyx/keys [job-id]} (listen model sub/?job-expr ::expr [:onyx/job-id] ::job-id job-catalog-id)]
    [re-com/h-box
      :children
      [
        [re-com/button
          :label "Submit"
          :on-click #(dispatch! model :onyx.sim.console.event/submit-job ::job-id job-catalog-id)]
        [re-com/input-text
          :model (str job-id)
          :on-change #(dispatch! model :onyx.sim.console.event/eav :e [:onyx.sim.api/job-id job-catalog-id] :a :onyx/job-id :v (or (to-uuid %) (gen-uuid)))
          :placeholder
          "Random Job ID"]]]))


(defn manage-job [model job-id] none
  (let [{:keys [onyx/doc ::title :onyx.core/workflow]}
        (listen model sub/?job-expr ::expr [:onyx.core/workflow :onyx/doc ::title] ::job-id job-id)]
    [re-com/v-box
      :children
      [
        [field-label "Title"]
        [re-com/label :label (str title)]
        [field-label "Catalog id"]
        [re-com/label :label (str job-id)]
        [field-label "Description"]
        [re-com/p (str doc)]
        [field-label "Workflow"]
        [code :code workflow]
        [submit-job-button model job-id]]]))

(defn job-catalog [model]
  (let [jobs (listen model sub/?job-catalog)]
    [re-com/v-box
      :children
      (into
        [[re-com/title
          :label "Job Catalog"
          :level :level1]]
        (comp
          (map
            (fn [[job-id]]
              [manage-job model job-id]))
          (interpose [re-com/gap :size "1rem"]))
        jobs)]))

(defn- codit [item]
  [code :code item])

(defn- eav-view [db]
  (let [eavs (d/datoms db :eavt)
        es (map first eavs)
        as (map second eavs)
        vs (map third eavs)]
    [re-com/h-box
      :children
      [
        [re-com/v-box :children (into [[field-label "Eid"]] (map codit es))]
        [re-com/v-box :children (into [[field-label "Attr"]] (map codit as))]
        [re-com/v-box :children (into [[field-label "Value"]] (map codit vs))]]]))

(defn- db-frisk [model]
  (let [conn (listen model sub/the-whole-conn)
        {:as db :keys [schema]} @conn]
    [re-com/v-box
      :children
      [
        [re-com/title
          :label "The DB"
          :level :level1]
        [field-label "Schema"]
        [code :code schema]
        [eav-view db]]]))

(defn running-jobs [model]
  (let [jobs (listen model sub/running-jobs)]
    [re-com/h-box
      :children
      (forv [job jobs]
        [code :code job])]))
    ;[job-view model job-id]

(defmulti display-selected 
  (fn [_ selection] 
    selection))

(defmethod display-selected ::settings
  [model _]
  [settings model])

(defmethod display-selected ::db-frisk
  [model _]
  [db-frisk model])

(defmethod display-selected ::job-catalog
  [model _]
  [job-catalog model])

(defmethod display-selected ::running-jobs
  [model _]
  [running-jobs model])

(defmethod display-selected :default
  [model ui-key]
  [re-com/p
    (str "TODO: " ui-key)])
  

(defn selected [model]
  (let [{::keys [selected-nav]} (listen model sub/?settings)]
    [re-com/box
     :class "onyx-sim"
     :child
     [display-selected model selected-nav]]))

(defn nav-bar [model]
  (let [choices (listen model sub/nav-tab-icons)
        selected-nav (listen model sub/selected-nav)]
    [re-com/horizontal-bar-tabs
      :tabs choices
      :model selected-nav
      :on-change #(dispatch! model :onyx.sim.console.event/eav :e [::name ::settings] :a ::selected-nav :v %)]))

(defn selector [model]
  [re-com/v-box
    :children
    [
      [re-com/gap :size ".25rem"]
      [re-com/h-box
        :style {:margin-left "auto"
                :margin-right "auto"}
        :align :center
        :gap "1ch"
        :children
        [
          [logo model]
          [nav-bar model]]]
      [re-com/gap :size ".25rem"]
      [selected model]]])
