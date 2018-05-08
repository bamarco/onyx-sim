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
  [{:db/ident ::env-display
    :db/valueType :db.type/ref}])
    

(def base-ui    
  [{:onyx/name ::settings
    ::selected-nav ::settings
    ::hidden? false
    ::description? true
    ::next-action? true
    ::env-display
    {::env-style ::pretty-env ;; ::raw-env
     ::render-segments? true
     :onyx/name ::env-display
     ::only-summary? true}
    ::animation-transitions [{:event :onyx.sim.api/tick}]
    ::frames-between-animation 30
    ::animating? false}])

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

(defn pull-settings [sim attrs]
  (pull sim attrs [:onyx/name ::settings]))

;;
;; q-exprs
;;
(def ?animating
  '[:find ?sim .
    :in $
    :where
    [?sim ::animating? true]])

(def ?running-jobs
  '[:find ?job-id
    :in $
    :where
    [?job :onyx.sim.api/job-id ?job-id]])

;;;
;;; Subscriptions
;;;
(defn- nav-choices [sim]
  (let [jobs (q ?running-jobs sim)
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
(defn env-style [sim]
  (let [{{::keys [only-summary? render-segments? env-style]} ::env-display} 
        (pull-settings sim
           '[{::env-display [::render-segments? ::only-summary? ::env-style]}])
        pretty? (= env-style ::pretty-env)
        raw?    (= env-style ::raw-env)]
    [re-com/v-box
      :children
      [
        [re-com/label :label "Environment Display Style" :class "field-label"]
        [re-com/radio-button
          :label "Pretty"
          :model env-style
          :value ::pretty-env
          :on-change #(update-eav! sim [:onyx/name ::env-display] ::env-style ::pretty-env)]
        [re-com/checkbox
          :label "(Render Segments)"
          :model render-segments?
          :disabled? raw?
          :on-change #(update-eav! sim [:onyx/name ::env-display] ::render-segments? (not render-segments?))]
        [re-com/radio-button
          :label "Raw"
          :model env-style
          :value ::raw-env
          :on-change #(update-eav! sim [:onyx/name ::env-display] ::env-style ::raw-env)]
        [re-com/checkbox
          :label "(Only Summary)"
          :model only-summary?
          :disabled? pretty?
          :on-change #(update-eav! sim [:onyx/name ::env-display] ::only-summary? (not only-summary?))]]]))

(defn settings [{:as sim :keys [conn]}]
  (let [{::keys [hidden? description? next-action?]}
        (pull-settings sim [::hidden? ::description? ::next-action?])]
    [re-com/v-box
      :children
      [
        [re-com/title
          :label "Settings"
          :level :level1]
        [re-com/checkbox 
          :model hidden?
          :label "Show Task Hider"
          :on-change #(update-setting! sim ::hidden? (not hidden?))]
        [re-com/checkbox 
          :model description?
          :label "Show Sim Description"
          :on-change #(update-setting! sim ::description? (not description?))]
        [re-com/checkbox 
          :model next-action?
          :label "Show Next Action"
          :on-change #(update-setting! sim ::next-action? (not next-action?))]
        [env-style sim]]]))


(defn job-view [sim job-id])

(defn third [coll]
  (nth coll 2))

(defn- codit [item]
  [code :code item])

(defn- box-it [item]
  [re-com/box :child item])

(defn- eav-view [db]
  (let [eavs (d/datoms db :eavt)
        es (map first eavs)
        as (map second eavs)
        vs (map third eavs)]
    [re-com/h-box
      :children
      [
        [re-com/v-box :children (into [[re-com/label :class "field-label" :label "Eid"]] (map codit es))]
        [re-com/v-box :children (into [[re-com/label :class "field-label" :label "Attr"]] (map codit as))]
        [re-com/v-box :children (into [[re-com/label :class "field-label" :label "Value"]] (map codit vs))]]]))

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
            [re-com/label :class "field-label" :label "Schema"]
            [code :code schema]
            [eav-view db]]]))))

(defmulti display-selected (fn [_ selection] selection))

; (defmethod display-selected
;   ::sim-view
;   [conn _]
;   [sim-view conn {::sim (selected-sim conn)}])

(defmethod display-selected
  ::settings
  [conn _]
  [settings conn])

(defmethod display-selected
  ::db-view
  [sim _]
  [db-view sim])

; (defmethod display-selected
;   :sims
;   [conn _]
;   [manage-sims conn])

(defmethod display-selected
  :default
  [sim selected]
  [job-view sim selected])

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
