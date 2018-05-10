(ns onyx.sim.ui
  (:require
    [re-com.core :as re-com]
    [taoensso.timbre :as log]
    [posh.reagent :as posh]
    [onyx.sim.api :as api]
    [datascript.core :as d]
    [reagent.ratom :as ratom]
    [onyx.sim.utils :as utils :refer [cat-into deref-or-value ppr-str mapply]]))

(def schema-idents
  [])

(def base-ui    
  [{:onyx/name ::settings
    ::selected-nav ::settings
    ::task-hider? true
    ::description? false
    ::next-action? true
    ::hidden-tasks #{}
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

;;;
;;; Queries
;;;
(defn q [q-expr {:keys [conn]} & ins]
  @(apply posh/q q-expr conn ins))

(defn pull [{:keys [conn]} pull-expr eid]
  @(posh/pull conn pull-expr eid))

(defn pull-attr [{:keys [conn]} attr eid]
  (get
    @(posh/pull conn [attr] eid)
    attr))

(defn pull-setting [sim attr]
  (pull-attr sim attr [:onyx/name ::settings]))

(defn pull-from-job [{:as sim ::keys [selected-job-id]} attrs]
  (pull sim attrs [:onyx/job-id selected-job-id]))

(defn pull-settings [sim attrs]
  (pull sim attrs [:onyx/name ::settings]))

(defn pull-env [{:keys [envs ::selected-job-id]}]
  (get @envs selected-job-id))

;;
;; q-exprs
;;
(def ?animating
  '[:find ?sim .
    :in $
    :where
    [?sim ::animating? true]])

(def ?jobs
  '[:find ?job-id
    :in $
    :where
    [?job :onyx/job-id ?job-id]])

;;;
;;; Subscriptions
;;;
(defn- nav-choices [sim]
  (let [jobs (q ?jobs sim)
        sims (for [[job-id] jobs]
               {:id job-id
                :label "temp"})]
    (cat-into
     [{:id ::settings
       :label [:i {:class "zmdi zmdi-settings"}]}]
     sims
     [{:id ::jobs
       :label [:i {:class "zmdi zmdi-widgets"}]}
      {:id ::db-view
       :label [:i {:class "zmdi zmdi-assignment"}]}])))

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
  (log/info (ppr-str args))
  [re-com/label :label (apply ppr-str args)])

(defn logo [sim]
  (let [animating? (q ?animating sim)]
    ; [:div (str @(:conn sim))]))
    [active-logo
     :img "ns/onyx/onyx-logo.png"
     :active? animating?
     :label "nyx-sim (alpha)"]))
  
(defn field-label [label]
  [re-com/label
    :class "field-label"
    :label label])

;;;
;;; Events
;;;
(defn update-eav! [{:keys [conn]} eid attr value]
  (d/transact!
    conn
    [[:db/add eid attr value]]))

(defn update-setting! [sim attr value]
  (update-eav! sim [:onyx/name ::settings] attr value))

;;;
;;; Simulator Components
;;;
(defn- env-style [sim]
  (let [{::keys [only-summary? render-segments? env-style]} 
        (pull-settings sim '[::render-segments? ::only-summary? ::env-style])
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
          :on-change #(update-setting! sim ::env-style ::pretty-env)]
        [re-com/checkbox
          :label "(Render Segments)"
          :model render-segments?
          :disabled? raw?
          :on-change #(update-setting! sim ::render-segments? (not render-segments?))]
        [re-com/radio-button
          :label "Raw"
          :model env-style
          :value ::raw-env
          :on-change #(update-setting! sim ::env-style ::raw-env)]
        [re-com/checkbox
          :label "(Only Summary)"
          :model only-summary?
          :disabled? pretty?
          :on-change #(update-setting! sim ::only-summary? (not only-summary?))]]]))

(defn- settings [{:as sim :keys [conn]}]
  (let [{::keys [task-hider? description? next-action?]}
        (pull-settings sim [::task-hider? ::description? ::next-action?])]
    [re-com/v-box
      :children
      [
        [re-com/title
          :label "Settings"
          :level :level1]
        [re-com/checkbox 
          :model task-hider?
          :label "Show Task Hider"
          :on-change #(update-setting! sim ::task-hider? (not task-hider?))]
        [re-com/checkbox 
          :model description?
          :label "Show Job Description"
          :on-change #(update-setting! sim ::description? (not description?))]
        [re-com/checkbox 
          :model next-action?
          :label "Show Next Action"
          :on-change #(update-setting! sim ::next-action? (not next-action?))]
        [env-style sim]]]))

(defn- sorted-tasks [{:keys [envs ::selected-job-id]}]
  (vec
    (for [task (get-in @envs [selected-job-id :sorted-tasks])]
      {:id task
        :label (pr-str task)})))

(defn- hidden-tasks [sim]
  (pull-setting sim ::hidden-tasks))

(defn- hidden-tasks-view [sim]
  (let [choices (or (vec (sorted-tasks sim)) [])
        _ (log/info "choizez" choices)
        chosen  (hidden-tasks sim)]
        
    [re-com/v-box
      :children
      [
        [field-label "Hidden Tasks"]
        [re-com/selection-list
          :choices choices
          :model chosen
          ;; FIXME: hidden tasks should be per job
          :on-change (partial update-setting! sim ::hidden-tasks)]]]))

(defn- default-render [segs]
  [code :code segs])

(defn- pretty-outbox [sim task-name & {:keys [render]}]
  (let [{:as env :keys [tasks]} (pull-env sim)
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

(defn- pretty-inbox [sim task-name & {:keys [render]}]
  (let [{:keys [tasks]} (pull-env sim)
        {::keys [import-uris]} (pull-from-job sim '[::import-uris])
        inbox (get-in tasks [task-name :inbox])]
    [re-com/v-box
     :class "onyx-inbox"
     :children
     [[re-com/h-box
       :gap ".5ch"
       :align :center
       :children
       [[re-com/title
         :label "Inbox"
         :level :level3]
        [re-com/input-text
         :model (str (first import-uris))
         :on-change #()]
        [re-com/button
         :label "Import Segments"
         :on-click #()]]]
      [render inbox]]]))

(defn- pretty-task-box [sim task-name]
  (let [env (pull-env sim)
        {::keys [render]} (pull-from-job sim [::render])
        task-type (get-in env [:tasks task-name :event :onyx.core/task-map :onyx/type])
        task-doc (get-in env [:tasks task-name :event :onyx.core/task-map :onyx/doc])
        local-render (get-in env [:tasks task-name :event :onyx.core/task-map ::render])
        render-segments? (pull-setting sim ::render-segments?)
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
              :on-click #()]]]
        (when task-doc
          [re-com/label :style {:color :white} :label task-doc])
        [pretty-inbox sim task-name :render render-fn]
        [pretty-outbox sim task-name :render render-fn]]]))

(defn- pretty-env [sim]
  (let [{:as env :keys [sorted-tasks]} (pull-env sim)
        hidden-tasks (pull-setting sim ::hidden-tasks)]
    [re-com/v-box
     :class "onyx-env"
     :children
     (into
      []
      (for [task-name (remove (or hidden-tasks #{}) sorted-tasks)]
        ^{:key task-name}
        [pretty-task-box sim task-name]))]))

(defn- summary [sim & {:keys [summary-fn]}]
  (let [summary-fn (or summary-fn api/env-summary)
        env (pull-env sim)]
    [code :class "onyx-panel" :code (summary-fn env)]))

(defn- raw-env [sim]
  (let [only-summary? (pull-setting sim ::only-summary?)]
    [re-com/v-box
     :class "onyx-env"
     :children
     [(re-com/title
       :label "Raw Environment"
       :level :level3)
      [summary sim :summary-fn (when-not only-summary? identity)]]]))

(defn- next-action [sim]
  (let [{:as env :keys [next-action]} (pull-env sim)]
    [re-com/h-box
      :gap "1ch"
      :children
      [
        [re-com/label
          :class "field-label"
          :label "Next Action"]
        [re-com/label
          :label (str next-action)]]]))

(defn- description [sim]
  (let [{:onyx/keys [doc]} (pull-from-job sim '[:onyx/doc])]
    [re-com/p doc]))

(defn- action-bar [sim]
  ;; FIXME: running?
  (let [running? (q ?animating sim)]
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

(defn job-view [sim]
  (let [{::keys [env-style next-action? task-hider? description?]}
        (pull-settings sim [::env-style ::next-action? ::task-hider? ::description?])]
    [re-com/v-box
      :children
      [
        (when task-hider?
          [hidden-tasks-view sim])
        (when description?
          [description sim])
        (when next-action?
          [next-action sim])
        [action-bar sim]
        (case env-style
          ::pretty-env [pretty-env sim]
          ::raw-env    [raw-env sim]
          [warn "Unknown environment style" env-style])]]))

(defn manage-jobs [sim]
  [:div (str "jobs: " (q ?jobs sim))])

(defn third [coll]
  (nth coll 2))

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

(defn- db-view [{:keys [conn]}]
  (let [the-whole-conn (ratom/make-reaction #(deref conn))]
    (fn [_]
      (let [{:as db :keys [schema]} @the-whole-conn]
        [re-com/v-box
          :children
          [
            [re-com/title
              :label "The DB"
              :level :level1]
            [field-label "Schema"]
            [code :code schema]
            [eav-view db]]]))))

(defmulti display-selected 
  (fn [_ selection] 
    selection))

(defmethod display-selected
  ::settings
  [conn _]
  [settings conn])

(defmethod display-selected
  ::db-view
  [sim _]
  [db-view sim])

(defmethod display-selected
  ::jobs
  [conn _]
  [manage-jobs conn])

(defmethod display-selected
  :default
  [sim job-id]
  [job-view (assoc sim ::selected-job-id job-id)])

(defn selected [sim]
  (let [view (pull-setting sim ::selected-nav)]
    [re-com/box
     :class "onyx-sim"
     :child
      [display-selected sim view]]))

(defn nav-bar [sim]
  (let [choices (nav-choices sim)
        chosen  (pull-setting sim ::selected-nav)]
    [re-com/horizontal-bar-tabs
      :tabs choices
      :model chosen
      :on-change (partial update-setting! sim ::selected-nav)]))

(defn selector [sim]
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
          [logo sim]
          [nav-bar sim]]]
    [re-com/gap :size ".25rem"]
    [selected sim]]])
