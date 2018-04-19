(ns onyx.sim.event
  (:require [taoensso.timbre :as log]
            [datascript.core :as d]
            [onyx.sim.api :as onyx]
            [reagent.ratom :as ratom]
            [onyx.sim.utils :as utils :refer [ppr-str cat-into]]
            [cljs.core.async :refer [<! chan]]
            [cljs-http.client :as http]
            [reagent.core :as r])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(defmulti intent (fn [conn event inputs]
                   (let [intention (:dat.view/handler event)]
                     (assert intention "No :dat.view/handler set for intent.")
                     ;; (log/debug seg "Intenting" intention)
                     intention)))

(defn dispatch! [conn {:as event} & inputs]
  (d/transact!
    conn
    [[:db.fn/call
      intent
      event
      inputs]]))

(defn raw-dispatch! [conn {:as event} & inputs]
  (intent conn event inputs))

(def transitions-query '[:find ?sim ?transitions
                         :in $
                         :where
                         [?sim :onyx/type ::sim]
                         [?sim ::transitions ?transitions]])

(defn transitions-diff [transitions-before transitions-after]
  (let [[old-tss new-tss] (split-at (count transitions-before) transitions-after)]
    (assert (or (not transitions-before)
                (= old-tss transitions-before))
            (ex-info
              "Transition diff is unsafe"
              {:transitions-before transitions-before
               :transitions-after transitions-after}))
    new-tss))

(defn dispense [conn]
  (let [listeners (into {} (-> (meta conn)
                               :listeners
                               deref))
        env-atom ((::dispenser listeners) ::request-env-atom)]
    env-atom))

(defn subscribe-env [conn sim]
  (ratom/cursor
    (dispense conn)
    [(:db/id (d/entity @conn sim)) :env]))

(defn subscribe-clean-env [conn sim]
  (ratom/cursor
    (dispense conn)
    [(:db/id (d/entity @conn sim)) :clean-env]))

(defn sim-or-selected [db sim]
  (or (d/entity db sim)
      (-> db
          (d/entity [:onyx/name ::settings])
          ::selected-sim)))

(defn sim! [conn]
  ;; ???: env snapshotting of transitions?
  ;; ???: support undo transitions?
  (let [env-atom (r/atom {})]
    (d/listen!
      conn
      ::dispenser
      (fn [dispense-request]
        (when (keyword? dispense-request)
          env-atom)))
    (d/listen!
      conn
      ::envs
      (fn [{:as report :keys [db-before db-after]}]
        (let [;;env-atom (dispense db-after)
              sim->tss (d/q transitions-query db-after)]
          ;; TODO: do-seq each sim rather than swap! them all at once
          ;;         (log/info "sim->tss" sim->tss)
          (swap!
            env-atom
            (fn [envs]
              (reduce
                (fn [envs [sim tss-after]]
                  (let [{:keys [env transitions clean-env]} (get envs sim)]
                    (if (= transitions tss-after)
                      envs
                      (let [new-tss (transitions-diff transitions tss-after)]
;;                         (log/info "tss-after" tss-after)
                        (assoc
                          envs
                          sim
                          {:env (reduce onyx/transition-env env new-tss)
                           :transitions tss-after
                           :clean-env (or clean-env (onyx/transition-env nil (first tss-after)))})))))
                envs
                sim->tss))))))
    (d/listen!
      conn
      ::animating
      (fn [{:as report :keys [db-before db-after]}]
        (let [settings-before (d/entity db-before [:onyx/name ::settings])
              settings-after  (d/entity db-after [:onyx/name ::settings])
              animating? (::animating? settings-after)
              was-animating? (::animating? settings-before)
              touched? (not= (::touch settings-before) (::touch settings-after))
              start-animating? (and (not was-animating?) animating?)]
          (when (or start-animating? (and animating? touched?))
            ;; TODO: schedule a callback when frames-between-animation is large instead of using r/next-tick
            (r/next-tick #(dispatch! conn {:dat.view/handler ::animate-sims}))))))))

(defn unsim! [conn]
  (dispatch! conn {:dat.view/handler ::stop-animate-sims})
  (d/unlisten! conn ::dispenser)
  (d/unlisten! conn ::envs)
  (d/unlisten! conn ::animating))

(defmethod intent ::animate-sims
  [db _ _]
  (let [{:keys [db/id
                onyx.sim.core/animation-transitions
                onyx.sim.core/frames-between-animation
                onyx.sim.core/frames-since-last-animation
                onyx.sim.core/touch]}
        (d/pull db [:db/id
                    ::animation-transitions
                    ::frames-between-animation
                    ::frames-since-last-animation
                    ::touch] [:onyx/name ::settings])
        animate? (= (or frames-since-last-animation 0) (or frames-between-animation 0))]
    (into
      [[:db/add id ::touch (not touch)]]
      (if-not animate?
        [[:db/add id ::frames-since-last-animation (inc frames-since-last-animation)]]
        (into
          [[:db/add id ::frames-since-last-animation 0]]
          (for [[sim transitions] (d/q transitions-query db)]
            [:db/add sim ::transitions (into transitions animation-transitions)]))))))

(defmethod intent ::stop-animate-sims [db _ _]
  ;; TODO: stop all ::running?
    [[:db/add [:onyx/name ::settings] ::animating? false]])

(defn ^:export simple-toggle [db {:keys [dat.view/entity dat.view/attr]}]
  (let [old-value (attr (d/entity db entity))
        new-value (not old-value)]
    [(when old-value [:db/retract entity attr old-value])
     [:db/add entity attr new-value]]))

(defmethod intent
  ::simple-toggle
  [db event _]
  [[:db.fn/call simple-toggle event]])

(defn ^:export simple-value [db {:keys [dat.view/entity dat.view/attr dat.view/value]}]
  (let [old-value (attr (d/entity db entity))]
;;     (log/info "set eav" [entity attr value])
    [(when old-value [:db/retract entity attr old-value])
     [:db/add entity attr value]]))

(defmethod intent
  ::simple-value
  [db event [value]]
  [[:db.fn/call simple-value (assoc event :dat.view/value value)]])

(defn hide-tasks [db {:keys [onyx.sim.core/sim onyx.sim.core/task-names]}]
  (let [sim (sim-or-selected db sim)]
    [[:db/add (:db/id sim) ::hidden-tasks task-names]]))

(defn hide-task [db {:keys [onyx.sim.core/sim onyx.sim.core/task-name]}]
  (let [sim (sim-or-selected db sim)
        hidden-tasks (::hidden-tasks sim)]
    [[:db/add (:db/id sim) ::hidden-tasks (conj hidden-tasks task-name)]]))

(defmethod intent
  ::hide-tasks
  [db event [task-names]]
  [[:db.fn/call hide-tasks
    (assoc
      event
      ::task-names task-names)]])

(defmethod intent
  ::hide-task
  [db event _]
  [[:db.fn/call hide-task event]])

(defmethod intent
  ::toggle-play
  [db {:keys [onyx.sim.core/sim]} _]
  (let [sim (sim-or-selected db sim)
        sim-id (:db/id sim)
        was-running? (::running? sim)
        is-running? (not was-running?)
        ;; TODO: last-to-stop? should check all sims to see if they are running
        animating? (::animating? (d/entity db [:onyx/name ::settings]))
        last-to-stop? was-running?
        first-to-start? is-running?]
;;     (log/info "toggle-event" {:first first-to-start? :last last-to-stop?})
    (cat-into
      [[:db/retract sim-id ::running? was-running?]
       [:db/add sim-id ::running? is-running?]]
      (when last-to-stop?
        [[:db/retract [:onyx/name ::settings] ::animating? true]
         [:db/add [:onyx/name ::settings] ::animating? false]])
      (when first-to-start?
        [[:db/retract [:onyx/name ::settings] ::animating? false]
         [:db/add [:onyx/name ::settings] ::animating? true]]))))

(defmethod intent
  ::select-view
  [db _ [selected]]
  (if (keyword? selected)
    [[:db/add [:onyx/name ::settings] ::selected-view selected]]
    [{:db/id [:onyx/name ::settings]
      ::selected-sim selected
      ::selected-view ::sim-view}]))

(defmethod intent
  ::transitions
  [db {:as event :keys [onyx.sim.core/sim onyx.sim.core/transitions]} _]
  (let [sim (:db/id (sim-or-selected db sim))
        tss (-> db
                (d/entity sim)
                ::transitions)]
    [(when tss [:db/retract sim ::transitions tss])
     [:db/add sim ::transitions (into (or tss []) transitions)]]))

(defmethod intent
  ::import-segments
  [conn {:keys [onyx.sim.core/sim onyx.sim.core/task-name]} _]
  (let [uri (or (::import-uri (d/entity @conn sim)) (first (::import-uris (d/entity @conn sim))))]
    (go
      (let [response (<! (http/get uri))]
        (log/info (str "retrieving edn from <" uri ">"))
        (log/debug "edn is...\n" (ppr-str (:body response)))
        (dispatch! conn {:dat.view/handler ::transitions
                         ::transitions [{:event :onyx.sim.api/inputs
                                                 :inputs {task-name (:body response)}}]})))))

