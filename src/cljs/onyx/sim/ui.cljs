(ns onyx.sim.ui
  (:require
    [re-com.core :as re-com]
    [taoensso.timbre :as log]
    [posh.reagent :as posh]
    [onyx.sim.api :as api]
    [onyx.sim.event2 :as event]
    [onyx.sim.kb :as kb]
    [onyx.sim.sub :as sub]
    [datascript.core :as d]
    [onyx.sim.kb :as kb]
    [reagent.ratom :as ratom]
    [clojure.core.async :as async :refer [go go-loop >! <! alts!]]
    [onyx.sim.utils :as utils :refer [third deref-or-value ppr-str mapply]]))

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

(defn sub [{:as sim :keys [knowbase]} & args]
  (apply kb/sub knowbase args))

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
;;; Simulator Components
;;;
(defn logo [sim]
  (let [animating? @(sub sim sub/?animating)]
    ; (log/info "animating?" animating?)
    [active-logo
      :img "ns/onyx/onyx-logo.png"
      :active? animating?
      :label "nyx-sim (alpha)"]))

(defn- env-style [sim]
  (let [{::keys [only-summary? render-segments? env-style]} @(sub sim sub/?settings)
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
          :on-change #(event/update-setting! sim ::env-style ::pretty-env)]
        [re-com/checkbox
          :label "(Render Segments)"
          :model render-segments?
          :disabled? raw?
          :on-change #(event/update-setting! sim ::render-segments? (not render-segments?))]
        [re-com/radio-button
          :label "Raw"
          :model env-style
          :value ::raw-env
          :on-change #(event/update-setting! sim ::env-style ::raw-env)]
        [re-com/checkbox
          :label "(Only Summary)"
          :model only-summary?
          :disabled? pretty?
          :on-change #(event/update-setting! sim ::only-summary? (not only-summary?))]]]))

(defn- settings [{:as sim :keys [conn knowbase]}]
  (let [{::keys [task-hider? description? next-action?]} @(sub sim sub/?settings)]
    [re-com/v-box
      :children
      [
        [re-com/title
          :label "Settings"
          :level :level1]
        [re-com/checkbox 
          :model task-hider?
          :label "Show Task Hider"
          :on-change #(event/update-setting! sim ::task-hider? (not task-hider?))]
        [re-com/checkbox 
          :model description?
          :label "Show Job Description"
          :on-change #(event/update-setting! sim ::description? (not description?))]
        [re-com/checkbox 
          :model next-action?
          :label "Show Next Action"
          :on-change #(event/update-setting! sim ::next-action? (not next-action?))]
        [env-style sim]]]))

(defn- hidden-tasks-view [{:as sim :keys [knowbase]}]
  (let [choices (or (vec (sub/sorted-task-labels knowbase)) [])
        {::keys [hidden-tasks]} @(sub sim sub/?hidden-tasks)
        hidden-tasks (or hidden-tasks #{})
        job-id (get-in sim [:knowbase ::job-id])]
    [re-com/v-box
      :children
      [
        [field-label "Hidden Tasks"]
        [re-com/selection-list
          :choices choices
          :model hidden-tasks
          :on-change #(event/update-eav! sim [:onyx/job-id job-id] ::hidden-tasks %)]]]))

(defn- default-render [segs]
  [code :code segs])

(defn- pretty-outbox [sim task-name & {:keys [render]}]
  (let [{:as env :keys [tasks]} @(sub sim sub/?env)
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
  (let [{:keys [tasks]} @(sub sim sub/?env)
        {::keys [import-uris]} @(sub sim sub/?import-uris)
        inbox (get-in tasks [task-name :inbox])
        import-uri (first import-uris)]
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
         :model (str import-uri)
         :on-change #(event/update-eav! sim
                      [:onyx/job-id (get-in sim [:knowledge ::job-id])]
                      ::import-uris (-> import-uris
                                      (disj import-uri)
                                      (conj %)))]
        [re-com/button
         :label "Import Segments"
         :on-click #()]]]
                     
      [render inbox]]]))

(defn- pretty-task-box [sim task-name]
  (let [env @(sub sim sub/?env)
        {::keys [render]} @(sub sim sub/?job-expr ::expr [::render])
        task-type (get-in env [:tasks task-name :event :onyx.core/task-map :onyx/type])
        task-doc (get-in env [:tasks task-name :event :onyx.core/task-map :onyx/doc])
        local-render (get-in env [:tasks task-name :event :onyx.core/task-map ::render])
        {::keys [render-segments?]} @(sub sim sub/?settings)
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
              :on-click #(event/hide-task! sim task-name)]]]
        (when task-doc
          [re-com/label :style {:color :white} :label task-doc])
        [pretty-inbox sim task-name :render render-fn]
        [pretty-outbox sim task-name :render render-fn]]]))

(defn- pretty-env [sim]
  (let [{:as env :keys [sorted-tasks]} @(sub sim sub/?env)
        {::keys [hidden-tasks]} @(sub sim sub/?hidden-tasks)]
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
        env @(sub sim sub/?env)]
    [code :class "onyx-panel" :code (summary-fn env)]))

(defn- raw-env [sim]
  (let [{::keys [only-summary?]} @(sub sim sub/?settings)]
    [re-com/v-box
     :class "onyx-env"
     :children
     [(re-com/title
       :label "Raw Environment"
       :level :level3)
      [summary sim :summary-fn (when-not only-summary? identity)]]]))

(defn- next-action [sim]
  (let [{:as env :keys [next-action]} @(sub sim sub/?env)]
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
  (let [{:onyx/keys [doc]} @(sub sim sub/?job-expr ::expr [:onyx/doc])]
    [re-com/p doc]))

(defn- action-bar [sim]
  ;; FIXME: running?
  (let [running? @(sub sim sub/?animating)]
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
  (let [{::keys [env-style next-action? task-hider? description?]} @(sub sim sub/?settings)]
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
          

(defn manage-job-view [sim]
  (let [{:keys [onyx/job-id onyx/doc ::title :onyx.core/workflow]}
        @(sub sim sub/?job-expr ::expr [:onyx.core/workflow :onyx/job-id :onyx/doc ::title])]
    [re-com/v-box
      :children
      [
        [field-label "Title"]
        [re-com/label :label (str title)]
        [field-label "Job-id"]
        [re-com/label :label (str job-id)]
        [field-label "Description"]
        [re-com/p (str doc)]
        [field-label "Workflow"]
        [code :code workflow]]]))

(defn manage-jobs [sim]
  (let [jobs @(sub sim sub/?jobs)]
    [re-com/v-box
      :children
      (into
        [[re-com/title
          :label "Job Manager"
          :level :level1]]
        (comp
          (map
            (fn [[job-id]]
              [manage-job-view (assoc-in sim [:knowbase ::job-id] job-id)]))
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

(defn- db-view [sim]
  ; (log/info "db-view")
  (let [the-whole-conn (ratom/make-reaction #(deref (get-in sim [:knowbase :db :conn])))]
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
  [sim _]
  [settings sim])

(defmethod display-selected
  ::db-view
  [sim _]
  [db-view sim])

(defmethod display-selected
  ::jobs
  [sim _]
  [manage-jobs sim])

(defmethod display-selected
  :default
  [sim job-id]
  [job-view (assoc-in sim [:knowbase ::job-id] job-id)])

(defn selected [sim]
  (let [{::keys [selected-nav]} @(sub sim sub/?settings)]
    [re-com/box
     :class "onyx-sim"
     :child
      [display-selected sim selected-nav]]))

(defn nav-bar [{:as sim :keys [knowbase]}]
  (let [choices (sub/nav-choices knowbase)
        {:as ss ::keys [selected-nav]} @(sub sim sub/?settings)]
    [re-com/horizontal-bar-tabs
      :tabs choices
      :model selected-nav
      :on-change (partial event/update-setting! sim ::selected-nav)]))

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
