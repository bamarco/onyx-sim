(ns onyx.sim.event
  (:require [taoensso.timbre :as log]
            [dat.sync.db :as d :refer [pull q]]
            [datascript.core :as ds]
            [onyx.sim.api :as onyx]
            [reagent.ratom :as ratom]
            [onyx.sim.utils :as utils :refer [ppr-str cat-into]]
            [cljs.core.async :refer [<! chan]]
            [cljs-http.client :as http]
            [reagent.core :as r])
  (:require-macros [cljs.core.async.macros :refer [go]]))


(defmulti intent2 (fn [conn event inputs]
                   (let [intention (:dat.view/handler event)]
                     (assert intention "No :dat.view/handler set for intent.")
                     ;; (log/debug seg "Intenting" intention)
                     intention)))

(def tx-middleware d/mw-keep-meta)

(defn dispatch! [conn {:as event} & inputs]
  (d/transact!
    conn
    [[:db.fn/call
      intent2
      event
      inputs]]
    {:datascript.db/tx-middleware tx-middleware}))

(defn raw-dispatch! [conn {:as event} & inputs]
  (intent2 conn event inputs))

(def transitions-query '[:find ?sim ?transitions
                        :in $
                        :where
                        [?sim :onyx/type :onyx.sim/sim]
                        [?sim :onyx.sim/transitions ?transitions]])

(defn transitions-diff [transitions-before transitions-after]
  (let [[old-tss new-tss] (split-at (count transitions-before) transitions-after)]
;;     (log/info "tss-diff" {:before transitions-before
;;                           :after transitions-after
;;                           :old old-tss
;;                           :new new-tss})
    (assert (or (not transitions-before)
                (= old-tss transitions-before))
            (ex-info
              "Transition diff is unsafe"
              {:transitions-before transitions-before
               :transitions-after transitions-after}))
    new-tss))

(defn sim-transitioner [db sim]
  (fn [env transition]
;;     (log/info "transitioning")
    (onyx/transition-env
      env
      (assoc transition
        :db db
        :sim sim))))

(defn dispense [db]
  (::env-atom (meta db)))

(defn listen-env [db sim]
  (ratom/cursor
    (dispense db)
    [(:db/id (d/entity db sim)) :env]))

(defn sim-or-selected [db sim]
  (or (d/entity db sim)
      (-> db
          (d/entity [:onyx/name :onyx.sim/settings])
          :onyx.sim/selected-sim)))

(defmethod intent2 ::animate-sims
  [db _ _]
  (let [{:keys [db/id
                onyx.sim/animation-transitions
                onyx.sim/frames-between-animation
                onyx.sim/frames-since-last-animation
                onyx.sim/touch]}
        (d/pull db [:db/id
                    :onyx.sim/animation-transitions
                    :onyx.sim/frames-between-animation
                    :onyx.sim/frames-since-last-animation
                    :onyx.sim/touch] [:onyx/name :onyx.sim/settings])
        animate? (= (or frames-since-last-animation 0) (or frames-between-animation 0))]
    (into
      [[:db/add id :onyx.sim/touch (not touch)]]
      (if-not animate?
        [[:db/add id :onyx.sim/frames-since-last-animation (inc frames-since-last-animation)]]
        (into
          [[:db/add id :onyx.sim/frames-since-last-animation 0]]
          (for [[sim transitions] (d/q transitions-query db)]
            [:db/add sim :onyx.sim/transitions (into transitions animation-transitions)]))))))

(defn stop-animate-sims! [conn]
  (d/transact!
    conn
    [[:db/add [:onyx/name :onyx.sim/settings] :onyx.sim/animating? false]]))

(defn sim! [conn]
;;   (log/info "sim!")
  ;; ???: env snapshotting of transactions? Those ticks are gonna add up.
  ;; ???: support undo transitions?
  (swap!
    conn
    vary-meta
    (fn [m]
      (assoc
        (or m {})
        ::env-atom
        (r/atom {}))))
  (ds/listen!
    conn
    ::envs
    (fn [{:as report :keys [db-before db-after]}]
      (let [env-atom (dispense db-after)
            sim->tss (d/q transitions-query db-after)]
;;         (log/info "env-atom" env-atom sim->tss)
        (swap!
          env-atom
          (fn [envs]
;;             (log/info "reducing-tss")
            (reduce
              (fn [envs [sim tss-after]]
;;                 (log/info "tss-sim" sim tss-after)
                (let [{:keys [env transitions]} (get envs sim)]
;;                   (log/info "tss" {:before transitions
;;                                    :after tss-after})
                  (if (= transitions tss-after)
                    envs
                    (let [new-tss (transitions-diff transitions tss-after)]
;;                       (log/info "new-tss" new-tss)
                      (assoc
                        envs
                        sim
                        {:env (reduce (sim-transitioner db-after sim) env new-tss)
                         :transitions tss-after})))))
              envs
              sim->tss))))))
  (ds/listen!
    conn
    ::animating
    (fn [{:as report :keys [db-before db-after]}]
      (let [settings-before (d/entity db-before [:onyx/name :onyx.sim/settings])
            settings-after  (d/entity db-after [:onyx/name :onyx.sim/settings])
            animating? (:onyx.sim/animating? settings-after)
            was-animating? (:onyx.sim/animating? settings-before)
            touched? (not= (:onyx.sim/touch settings-before) (:onyx.sim/touch settings-after))
            start-animating? (and (not was-animating?) animating?)]
        (when (or start-animating? (and animating? touched?))
          ;; TODO: schedule a callback when frames-between-animation is large instead of using r/next-tick
          (r/next-tick #(dispatch! conn {:dat.view/handler ::animate-sims})))))))

(defn unsim! [conn]
  (stop-animate-sims! conn)
  (swap!
    conn
    vary-meta
    dissoc
    ::env-atom)
  (ds/unlisten! conn ::envs)
  (ds/unlisten! conn ::animating))

;; TODO: catch and display simulator errors.

(defmulti intent (fn [_ seg]
                   (let [intention (:onyx/type seg)]
                     (assert intention "No :onyx/type set for intent.")
                     ;; (log/debug seg "Intenting" intention)
                     intention)))

(defn dispatch-transition! [conn {:as event} & inputs]
  (d/transact!
    conn
    [[:onyx.sim.api/transition
      (assoc event
        :dat.view/inputs inputs)]]))

;; (defn raw-dispatch [conn seg]
;;   ;; TODO: make a nice error for when you didn't use raw-dispatch and you should have
;;   (intent conn seg))

(defn new-segments [env task-name segments]
  ;; TODO
  )

(defn mount-outputs! [env]
  (let [outputs (get-in env [:tasks :dat.view/mount :outputs])
        env (assoc-in env [:tasks :dat.view/mount :outputs] [])]
    (reduce
      (fn [env {:keys [dat.view/component dat.view/mount-point]}]
        (update-in
          env
          [:dat.view/in-flight mount-point]
          #(do
             (when (and mount-point (= % 1))
               (reset! mount-point component))
             (dec %))))
      env
      outputs)))

(defn pull-and-transition-env [db sim-id transition-or-transitions]
  (let [transitions (if (fn? transition-or-transitions)
                      [transition-or-transitions]
                      transition-or-transitions)
        env (:onyx.sim/env (pull db '[{:onyx.sim/env [*]}] sim-id))]
;;     (log/debug "transition" sim-id transitions)
    [(reduce (fn [env tr]
              (tr env)) env transitions)]))

(defn tick-sim [db {:as sim :keys [db/id onyx.sim/speed onyx.sim/pause-count]}]
  (let [next-pause-count (+ speed pause-count)
        tick-count (cond
                     (>= speed 1) speed
                     (>= next-pause-count 1) 1
                     :else 0)]
    (try
      (if (= tick-count 0)
        [;[:db/retract id :onyx.sim/pause-count pause-count]
         [:db/add id :onyx.sim/pause-count next-pause-count]]
        (pull-and-transition-env db id (repeat tick-count onyx/tick)))
      (catch :default e
        [[:db/add id :onyx.sim/debugging? true]]))))

(defn db-onyx-tick
  [db]
  (let [sims (d/q '[:find [(pull ?sim [:db/id :onyx.sim/speed :onyx.sim/running? :onyx.sim/pause-count :onyx.sim/debugging?]) ...]
                    :where
                    [?sim :onyx/type :onyx.sim/sim]] db)]
    (transduce
      (comp
        (filter :onyx.sim/running?)
        (remove :onyx.sim/debugging?)
        (map (partial tick-sim db)))
      into
      []
      sims)))

(defn ^:export simple-toggle [db {:keys [dat.view/entity dat.view/attr]}]
  (let [old-value (attr (d/entity db entity))
        new-value (not old-value)]
    [(when old-value [:db/retract entity attr old-value])
     [:db/add entity attr new-value]]))

(defmethod intent2
  ::simple-toggle
  [db event _]
  [[:db.fn/call simple-toggle event]])

(defn ^:export simple-value [db {:keys [dat.view/entity dat.view/attr dat.view/value]}]
  (let [old-value (attr (d/entity db entity))]
    (log/info "set eav" [entity attr value])
    [(when old-value [:db/retract entity attr old-value])
     [:db/add entity attr value]]))

(defmethod intent2
  ::simple-value
  [db event [value]]
    [[:db.fn/call simple-value (assoc event :dat.view/value value)]])

(defmethod intent
  :reagent/next-tick
  [db _]
  (let [sims (d/q '[:find ?sim ?running ?speed
                    :where
                    [?sim :onyx/type :onyx.sim/sim]
                    [?sim :onyx.sim/running? ?running]
                    [?sim :onyx.sim/speed ?speed]] db)]
    (reduce concat
    (for [[id running? speed] sims]
      (when running?
        ;; TODO: speed
        (pull-and-transition-env db id onyx/tick))))))

(defmethod intent
  :onyx.sim.control/env-style
  [db {:keys [:onyx.sim.control/selected]}]
  (let [pretty? (= selected :onyx.sim.control/pretty-env?)
        raw? (= selected :onyx.sim.control/raw-env?)]
    ;; ???: REPORT-BUG: There is a bug where radio buttons pass the value false instead of the selected value. We've hacked our way around it in this function, but this does not agree with the re-com documentation.
    ;; TODO: make a generic radio control handler and data type
;;     (log/debug selected "pretty? " pretty? " raw? " raw?)
    [[:db/add [:onyx/name :onyx.sim.control/raw-env?] :onyx.sim.control/toggled? raw?]
     [:db/add [:onyx/name :onyx.sim.control/pretty-env?] :onyx.sim.control/toggled? pretty?]]))

(defmethod intent
  :onyx.sim.control/toggled?
  [db {:keys [:onyx.sim/control :onyx.sim.control/toggled?]}]
  [[:db/add control :onyx.sim.control/toggled? toggled?]])

(defmethod intent
  :onyx.sim.event/hide-tasks
  [db {:keys [:onyx.sim/sim :onyx.sim/task-names]}]
  [[:db/add sim :onyx.sim/hidden-tasks task-names]])

(defn hide-tasks [db {:keys [onyx.sim/sim onyx.sim/task-names]}]
  (let [sim (sim-or-selected db sim)]
    (log/info "hidden-tasks" sim task-names)
    [[:db/add (:db/id sim) :onyx.sim/hidden-tasks task-names]]))

(defn hide-task [db {:keys [onyx.sim/sim onyx.sim/task-name]}]
  (let [sim (sim-or-selected db sim)
        hidden-tasks (:onyx.sim/hidden-tasks sim)]
    (log/info "hidden-task" sim hidden-tasks task-name)
    [[:db/add (:db/id sim) :onyx.sim/hidden-tasks (conj hidden-tasks task-name)]]))

(defmethod intent2
  ::hide-tasks
  [db event [task-names]]
  [[:db.fn/call
    hide-tasks
    (assoc
      event
      :onyx.sim/task-names task-names)]])

(defmethod intent2
  ::hide-task
  [db event _]
  (log/info "hiding" event)
  [[:db.fn/call hide-task event]])

(defmethod intent
  :onyx.sim.event/hide-task
  [db {:keys [:onyx.sim/sim :onyx.sim/task-name]}]
  (let [hidden-tasks (:onyx.sim/hidden-tasks (d/entity db sim))]
    [[:db/add sim :onyx.sim/hidden-tasks (conj hidden-tasks task-name)]]))

(defmethod intent
  :onyx.sim/transition-env
  [db {:keys [:onyx.sim/sim :onyx.sim/transition]}]
  (let [action
        (case transition
          :onyx.api/tick onyx/tick
          :onyx.api/step onyx/step
          :onyx.api/drain onyx/drain)]
    (pull-and-transition-env db sim action)))

(defmethod intent
  :onyx.sim.event/import-uri
  [db {:keys [:onyx.sim/sim :onyx.sim/task-name uri]}]
  [[:db/add sim :onyx.sim/import-uri uri]])

(defmethod intent
  :onyx.api/new-segment
  [db {:keys [onyx.sim/sim onyx.sim/task-name onyx.sim/segment]}]
  (pull-and-transition-env db sim #(onyx/new-segment % task-name segment)))

(defmethod intent
  :onyx.api/tick
  [db {:keys [:onyx.sim/sim]}]
  (pull-and-transition-env db sim onyx/tick))

(defmethod intent
  :onyx.api/step
  [db {:keys [:onyx.sim/sim]}]
  (pull-and-transition-env db sim onyx/step))

(defmethod intent
  :onyx.api/drain
  [db {:keys [:onyx.sim/sim]}]
  (pull-and-transition-env db sim onyx/drain))

;; (defn run-sim
;;   "DEPRECATED"
;;   [conn sim]
;;   ;; causes multi-ticking for all sims if one is running
;;   (let [running? (:onyx.sim/running? (d/entity @conn sim))]
;;     (when running?
;;       ;; FIXME: upgrade performance by doing everything inline
;;       (dispatch conn {:onyx/type :reagent/next-tick})
;;       (r/next-tick #(run-sim conn sim)))))

(defn run-sims [conn]
  (let [running? (d/q '[:find ?running .
                        :where
                        [?running :onyx.sim/running? true]]
                      @conn)]
    (when running?
      (d/transact! conn [[:db.fn/call db-onyx-tick]])
      (r/next-tick #(run-sims conn)))))

(defmethod intent
  :onyx.api/start
  [conn {:keys [:onyx.sim/sim]}]
  (do
    (d/transact! conn [[:db/add sim :onyx.sim/running? true]])
    (run-sims conn)))

(defmethod intent2
  ::toggle-play
  [db {:keys [onyx.sim/sim]} _]
  (let [sim (sim-or-selected db sim)
        sim-id (:db/id sim)
        was-running? (:onyx.sim/running? sim)
        is-running? (not was-running?)
        ;; TODO: last-to-stop? should check all sims to see if they are running
        animating? (:onyx.sim/animating? (d/entity db [:onyx/name :onyx.sim/settings]))
        last-to-stop? was-running?
        first-to-start? is-running?]
    (log/info "toggle-event" {:first first-to-start? :last last-to-stop?})
    (cat-into
      [[:db/retract sim-id :onyx.sim/running? was-running?]
       [:db/add sim-id :onyx.sim/running? is-running?]]
      (when last-to-stop?
        [[:db/retract [:onyx/name :onyx.sim/settings] :onyx.sim/animating? true]
         [:db/add [:onyx/name :onyx.sim/settings] :onyx.sim/animating? false]])
      (when first-to-start?
        [[:db/retract [:onyx/name :onyx.sim/settings] :onyx.sim/animating? false]
         [:db/add [:onyx/name :onyx.sim/settings] :onyx.sim/animating? true]]))))

;; (defmethod intent
;;   :onyx.api/stop
;;   [db {:keys [:onyx.sim/sim]}]
;;   [[:db/add sim :onyx.sim/running? false]])

;; (defmethod intent
;;   :onyx.sim/toggle-play
;;   [conn {:keys [:onyx.sim/sim]}]
;;   (let [running? (:onyx.sim/running? (d/entity @conn sim))]
;;     (if running?
;;       (d/transact! conn [[:db/add sim :onyx.sim/running? false]])
;;           (do
;;             (d/transact! conn [[:db/add sim :onyx.sim/running? true]])
;;             (run-sim conn sim)))))

(defmethod intent
  :onyx.sim/select-view
  [db {:keys [selected]}]
  (if (keyword? selected)
    [[:db/add [:onyx/name :onyx.sim/settings] :onyx.sim/selected-view selected]]
    [{:db/id [:onyx/name :onyx.sim/settings]
      :onyx.sim/selected-sim selected
      :onyx.sim/selected-view :onyx.sim/sim-view}]))

(defmethod intent2
  ::select-view
  [db _ [selected]]
  (if (keyword? selected)
    [[:db/add [:onyx/name :onyx.sim/settings] :onyx.sim/selected-view selected]]
    [{:db/id [:onyx/name :onyx.sim/settings]
      :onyx.sim/selected-sim selected
      :onyx.sim/selected-view :onyx.sim/sim-view}]))

(defmethod intent2
  ::transitions
  [db {:as event :keys [onyx.sim/sim onyx.sim/transitions]} _]
  (let [sim (:db/id (sim-or-selected db sim))
        tss (-> db
                (d/entity sim)
                :onyx.sim/transitions)]
    [(when tss [:db/retract sim :onyx.sim/transitions tss])
     [:db/add sim :onyx.sim/transitions (into (or tss []) transitions)]]))

(defmethod intent
  :onyx.sim.event/import-segments
  [conn {:keys [onyx.sim/sim onyx.sim/task-name]}]
  (let [uri (first (:onyx.sim/import-uris (d/entity @conn sim)))]
    (go (let [response (<! (http/get uri))]
          (log/info (str "retrieving edn from <" uri ">"))
          (log/debug "edn is...\n" (ppr-str (:body response)))
          (d/transact! conn [[:db.fn/call pull-and-transition-env sim
                               (for [seg (:body response)]
                                 #(onyx/new-segment % task-name seg))]])))))

(defmethod intent2
  ::import-segments
  [conn {:keys [onyx.sim/sim onyx.sim/task-name]} _]
  (let [uri (first (:onyx.sim/import-uris (d/entity @conn sim)))]
    (go
      (let [response (<! (http/get uri))]
        (log/info (str "retrieving edn from <" uri ">"))
        (log/debug "edn is...\n" (ppr-str (:body response)))
        (dispatch! conn {:dat.view/handler ::transitions
                         :onyx.sim/transitions [{:event :onyx.sim.api/inputs
                                                 :inputs {task-name (:body response)}}]})))))

