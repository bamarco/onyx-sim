(ns onyx.sim.control
  (:require [taoensso.timbre :as log]
            [onyx.sim.flui :as flui]
            [onyx.sim.event :as event]
            [onyx.sim.utils :as utils :refer [cat-into]]
            [datascript.core :as d]
;;             [onyx-local-rt.impl :refer [kw->fn]]
            [onyx.static.util :refer [kw->fn]]
            #?(:cljs [posh.reagent :as posh])))


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

(defn compile-control [conn control-spec]
  ;; FIXME: controls are compiling more than once and are only reactive inasmuch as they depend on reactive posh data. As is it works and is merely an efficiency leak. This is a good spot to check for efficiency gains later on.
  (into
    {}
    (for [[attr value] control-spec]
      (if (list? value)
        [attr
         (apply (kw->fn (first value)) conn (rest value))]
        [attr value]))))

(defn pull-control [conn control-name]
  (let [control (pull conn '[*] [:control/name control-name])]
    (log/info "compile-control" control)
    (compile-control conn control)))

(defn ^:export control-attr [conn control-name attr]
  (get
    (compile-control conn (pull conn [attr] [:control/name control-name]))
    attr))

;;
;; simple ds handlers
;;
(defn ^:export simple-toggle [conn control-name]
  ;; FIXME: should use dispatch
  #(d/transact!
     conn
     [[:db/add [:control/name control-name] :control/toggled? %]]))

(defn ^:export simple-choose-one [conn control-name]
  ;; FIXME: should use dispatch
  #(d/transact!
     conn
     [[:db/add [:control/name control-name] :control/chosen %]]))

;;
;; simple model fns
;;
(defn ^:export simple-chosen? [conn control-name choice]
  (let [chosen (control-attr conn control-name :control/chosen)]
    (contains? chosen choice)))

(defn ^:export simple-not-chosen? [conn control-name choice]
  (not (simple-chosen? conn control-name choice)))

;;
;; controls -> hiccup
;;
(defn field-label [conn control-name]
  [flui/label
   :class "field-label"
   :label (control-attr conn control-name :control/label)])

(defn action-button [conn control-name]
  (let [{:keys [:control/disabled? :control/action :control/label]} (pull-control conn control-name)]
    [flui/button
     :label label
     :disabled? disabled?
     :on-click action]))

(defn toggle-button [conn control-name]
  (let [{:keys [control/label control/toggle-label control/toggled? dat.view/event]} (pull-control conn control-name)]
    [flui/button
     :label (if toggled? (or toggle-label label) label)
     :on-click (partial event/dispatch! conn event)]))

(defn toggle-checkbox [conn control-name]
  (let [{:keys [control/disabled? control/label control/toggle-label control/toggled? dat.view/event]} (pull-control conn control-name)]
    [flui/checkbox
     :model toggled?
     :disabled? disabled?
     :label (if toggled? (or toggle-label label) label)
     :on-change (partial event/dispatch! conn event)]))

(defn selection-list [conn control-name]
  (let [{:keys [control/label control/choices control/chosen dat.view/event control/id-fn control/label-fn]} (pull-control conn control-name)]
    (log/info "selection-list" choices)
    (log/info "selection-list" chosen)
    [flui/v-box
     :children
     [[flui/label
       :class "field-label"
       :label label]
      [flui/selection-list
       :choices choices
       :model chosen
       :id-fn id-fn
       :label-fn label-fn
       :on-change (partial event/dispatch! conn event)]]]))

(defn indicator-label [conn control-name]
  (let [{:keys [:control/label :control/display]} (pull-control conn control-name)]
    [flui/h-box
     :gap "1ch"
     :children
     [[flui/label
       :class "field-label"
       :label label]
      [flui/label
       :label display]]]))

(defn active-logo [conn control-name]
  (let [{:keys [control/label control/img control/active?]} (pull-control conn control-name)]
    ;; FIXME: abrupt ending animation
    [flui/h-box
      :class "active-logo"
      :children
      [[flui/box :child [:img {:class (str "active-logo-img" (when active? " spinning"))
                               :src img}]]
       [flui/label :label label]]]))

(defn ^:export simple-concat [db {:as seg :keys [dat.view/entity dat.view/attr dat.view/inputs]}]
  (for [input inputs]
    [:db/add entity attr input]))

;; (defn ^:export toggle-select2 [db {:as seg :keys [dat.view/entity dat.view/attr dat.view/inputs]}]
;;   [[:db/retract entity attr (get (d/entity db entity) attr)]
;;    [:db/add entity attr #{(first inputs)}]])


(defn ^:export simple-value [db {:as seg :keys [dat.view/entity dat.view/attr dat.view/inputs]}]
  [[:db/retract entity attr (get (d/entity db entity) attr)]
   [:db/add entity attr (first inputs)]])

(defn ^:export simple-toggle2 [db {:as seg :keys [dat.view/entity dat.view/attr dat.view/inputs]}]
  ;; ???: allow entity to be :db/id of entity itself or eid
  (let [old-value (get (d/entity db entity) attr)]
    [[:db/retract entity attr old-value]
     [:db/add entity attr (not old-value)]]))

;; (defn ^:export single-as-set [{:as seg :keys [dat.view/inputs]}]
;;   (update-in
;;     seg
;;     [:dat.view/inputs]
;;     (fn [[selection]]

;;     ))


;;
;; dat.view/label or dat.view/label-fn and dat.view/choice
;; dat.view/choice-id or dat.view/id-fn and dat.view/choice
;;
(defn ^:export radio-choice2
  [{:keys [dat.view/dispatch! dat.view/conn]}
   {:keys [dat.view/label-fn
           dat.view/id-fn
           dat.view/option
           dat.view/selected
           dat.view/entity
           dat.view/attr
           dat.view/event]}]
  ;; ???: posh it
  (let [selected (or selected (get (d/entity conn entity) attr))
        label-fn (or label-fn :e/name)
        id-fn (or id-fn :db/id)]
    [flui/radio-button
     :model (id-fn selected)
     :value (id-fn option)
     :label (label-fn option)
     :on-change #(dispatch!
                   (into
                     {:dat.view/handler ::simple-value
                      :dat.view/entity entity
                      :dat.view/attr attr}
                     event)
                   (id-fn option))]))

(defn ^:export boxer [system {:as control :keys [dat.view/direction dat.view/children]}]
  (let [box (case direction
              :dat.view.container/horizontal flui/h-box
              flui/v-box)]
    [box
     :children
     (:dat.view/component children)]))

(defn ^:export radio-list
  [system
   {:as control
    :keys [dat.view/container dat.view/options dat.view/entity dat.view/attr]}]
  ;; TODO: sorting. maybe do sorting at container level.
  [boxer
   system
   (assoc
     container
     :dat.view/children
     (for [option options]
       (let [control (assoc control :dat.view/option option)
             component [radio-choice2 system control]]
         (assoc control :dat.view/component component))))])

;; (defn create-dispatcher [conn]
;;   (fn [{:as event :keys [dat.view/handler]} & inputs]
;;     (d/transact!
;;       conn
;;       [[:db.fn/call
;;         (onyx/kw->fn handler)
;;         (assoc
;;           event
;;           :dat.view/inputs inputs)]])))

;; (defn radio-choicer [conn control-name index]
;;   (let [{:keys [control/chosen control/choices]} (pull-control conn control-name)]
;;     [radio-choice2
;;      {:dat.view/conn conn
;;       :dat.view/dispatch!
;;       (create-dispatcher conn)}
;;      {:dat.view/label-fn :label
;;       :dat.view/id-fn :id
;;       :dat.view/option (get choices index)
;;       :dat.view/selected chosen
;;       :dat.view/entity [:control/name control-name]
;;       :dat.view/attr :control/toggled?}]))

(defn radio-choice [conn control-name index]
  (let [{:keys [control/label-fn control/id-fn control/chosen control/choices control/choose]} (pull-control conn control-name)
        label-fn (or label-fn :label)
        id-fn (or id-fn :id)
        choice (get choices index)]
    ;; ???: assert only one chosen
    [flui/radio-button
     :model (first chosen)
     :value (id-fn choice)
     :label (label-fn choice)
     :on-change #(choose #{(id-fn choice)})]))

(defn nav-bar [conn control-name]
  (let [{:keys [control/id-fn control/label-fn control/choices dat.view/event control/chosen]} (pull-control conn control-name)
        id-fn (or id-fn :id)
        label-fn (or label-fn :label)]
  (log/info "nav-bar" chosen)
    ;; ???: should the or-clause for id-fn be part of compile-controls?
    ;; ???: maybe a more generic way to do the bridging. drop nil arguments?
    [flui/horizontal-bar-tabs
     :tabs choices
     :model chosen ;; ???: treat chosen as a set always? distinction for choose one vs choose many?
     :id-fn id-fn
     :label-fn label-fn
     :on-change (partial event/dispatch! conn event)]))

(defn indicator-display [conn control-name]
  (let [{:keys [:control/display]} (pull-control conn control-name)]
    [flui/p display]))


;; FIXME: should be middleware/interceptors so you can use lots of this style
(defn when-show? [[component-fn conn control-name]]
  (if (control-attr conn control-name :control/show?)
    [component-fn conn control-name]
    flui/none))
