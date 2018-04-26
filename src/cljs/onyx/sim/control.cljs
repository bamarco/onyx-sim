(ns onyx.sim.control
  (:require [taoensso.timbre :as log]
            [re-com.core :as re-com]
            [onyx.sim.ui :as ui]
            [onyx.sim.event :as event]
            [onyx.sim.utils :as utils :refer [cat-into]]
            [datascript.core :as d]
            [onyx.static.util :refer [kw->fn]]
            [posh.reagent :as posh]))

;;;
;;; !!!: This experimental file is being refactored and will eventually be removed.
;;;

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
  (let [control @(posh/pull conn '[*] [:control/name control-name])]
;;     (log/info "compile-control" control)
    (compile-control conn control)))

(defn ^:export control-attr [conn control-name attr]
  (get
    (compile-control conn @(posh/pull conn [attr] [:control/name control-name]))
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
  [re-com/label
   :class "field-label"
   :label (control-attr conn control-name :control/label)])

(defn action-button [conn control-name]
  (let [{:keys [control/disabled? control/action control/label dat.view/event]} (pull-control conn control-name)]
    [re-com/button
     :label label
     :disabled? disabled?
     :on-click (partial event/dispatch! conn event)]))

(defn toggle-button [conn control-name]
  (let [{:keys [control/label control/toggle-label control/toggled? dat.view/event]} (pull-control conn control-name)]
    [re-com/button
     :label (if toggled? (or toggle-label label) label)
     :on-click (partial event/dispatch! conn event)]))

(defn toggle-checkbox [conn control-name]
  (let [{:keys [control/disabled? control/label control/toggle-label control/toggled? dat.view/event]} (pull-control conn control-name)]
    [re-com/checkbox
     :model toggled?
     :disabled? disabled?
     :label (if toggled? (or toggle-label label) label)
     :on-change (partial event/dispatch! conn event)]))

(defn selection-list [conn control-name]
;;   (log/info "selection-list" control-name)
  (let [{:keys [control/label control/choices control/chosen dat.view/event control/id-fn control/label-fn]} (pull-control conn control-name)]
;;     (log/info "label:" label "chosen:" chosen "choices:" choices)
    [re-com/v-box
     :children
     [[re-com/label
       :class "field-label"
       :label label]
      [re-com/selection-list
       :choices choices
       :model chosen
       :id-fn id-fn
       :label-fn label-fn
       :on-change (partial event/dispatch! conn event)]]]))

(defn indicator-label [conn control-name]
  (let [{:keys [control/label control/display]} (pull-control conn control-name)]
    [re-com/h-box
     :gap "1ch"
     :children
     [[re-com/label
       :class "field-label"
       :label label]
      [re-com/label
       :label display]]]))

(defn active-logo [conn control-name]
  (let [{:keys [control/label control/img control/active?]} (pull-control conn control-name)]
    ;; FIXME: abrupt ending animation
    [re-com/h-box
      :class "active-logo"
      :children
      [[re-com/box :child [:img {:class (str "active-logo-img" (when active? " spinning"))
                                 :src img}]]
       [re-com/label :label label]]]))

(defn ^:export simple-concat [db {:as seg :keys [dat.view/entity dat.view/attr dat.view/inputs]}]
  (for [input inputs]
    [:db/add entity attr input]))

(defn ^:export simple-value [db {:as seg :keys [dat.view/entity dat.view/attr dat.view/inputs]}]
  [[:db/retract entity attr (get (d/entity db entity) attr)]
   [:db/add entity attr (first inputs)]])

(defn ^:export simple-toggle2 [db {:as seg :keys [dat.view/entity dat.view/attr dat.view/inputs]}]
  ;; ???: allow entity to be :db/id of entity itself or eid
  (let [old-value (get (d/entity db entity) attr)]
    [[:db/retract entity attr old-value]
     [:db/add entity attr (not old-value)]]))

;;
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
    [re-com/radio-button
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
              :dat.view.container/horizontal re-com/h-box
              re-com/v-box)]
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

(defn radio-choice [conn control-name index]
  (let [{:keys [control/label-fn control/id-fn control/chosen control/choices control/choose]} (pull-control conn control-name)
        label-fn (or label-fn :label)
        id-fn (or id-fn :id)
        choice (get choices index)]
    ;; ???: assert only one chosen
    [re-com/radio-button
     :model (first chosen)
     :value (id-fn choice)
     :label (label-fn choice)
     :on-change #(choose #{(id-fn choice)})]))

(defn nav-bar [conn control-name]
  (let [{:keys [control/id-fn control/label-fn control/choices dat.view/event control/chosen]} (pull-control conn control-name)
        id-fn (or id-fn :id)
        label-fn (or label-fn :label)]
;;   (log/info "nav-bar" control-name " chosen: " chosen " choices: " choices)
    ;; ???: should the or-clause for id-fn be part of compile-controls?
    ;; ???: maybe a more generic way to do the bridging. drop nil arguments?
    [re-com/horizontal-bar-tabs
     :tabs choices
     :model chosen ;; ???: treat chosen as a set always? distinction for choose one vs choose many?
     :id-fn id-fn
     :label-fn label-fn
     :on-change (partial event/dispatch! conn event)]))

(defn indicator-display [conn control-name]
  (let [{:keys [:control/display]} (pull-control conn control-name)]
    [re-com/p display]))


;; FIXME: should be middleware/interceptors so you can use lots of this style
(defn when-show? [[component-fn conn control-name]]
  (if (control-attr conn control-name :control/show?)
    [component-fn conn control-name]
    ui/none))
