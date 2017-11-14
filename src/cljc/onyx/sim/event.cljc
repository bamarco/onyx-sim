(ns onyx.sim.event
  (:require [taoensso.timbre :as log]
            [dat.sync.db :as d :refer [pull q]]
            [onyx.sim.api :as onyx]
            [onyx.sim.utils :as utils :refer [ppr-str cat-into]]
            #?(:cljs [cljs.core.async :refer [<! chan]])
            #?(:cljs [cljs-http.client :as http])
            #?(:cljs [reagent.core :as r]))
  #?(:cljs (:require-macros [cljs.core.async.macros :refer [go]])))

;; TODO: catch and display simulator errors.

(defmulti intent (fn [_ seg]
                   (let [intention (:onyx/type seg)]
                     (assert intention "No :onyx/type set for intent.")
                     ;; (log/debug seg "Intenting" intention)
                     intention)))

(defmulti intent2 (fn [conn event inputs]
                   (let [intention (:dat.view/handler event)]
                     (assert intention "No :dat.view/handler set for intent.")
                     ;; (log/debug seg "Intenting" intention)
                     intention)))

(defn dispatch [conn seg]
  (d/transact! conn [[:db.fn/call intent seg]]))

;; (defn dispatch [conn seg]
;;   (d/transact!
;;     conn
;;     [[:onyx.sim.api/transition seg]]
;;     {:datascript.db/tx-middleware onyx/middleware}))

(defn dispatch! [conn {:as event} & inputs]
  (d/transact!
    conn
    [[:db.fn/call
      intent2
      event
      inputs]]
    {:datascript.db/tx-middleware onyx/tx-middleware}))

(defn dispatch-transition! [conn {:as event} & inputs]
  (d/transact!
    conn
    [[:onyx.sim.api/transition
      (assoc event
        :dat.view/inputs inputs)]]))

(defn raw-dispatch [conn seg]
  ;; TODO: make a nice error for when you didn't use raw-dispatch and you should have
  (intent conn seg))

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
      (catch #?(:clj Error :cljs :default) e
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
    [[:db/retract entity attr old-value]
     [:db/add entity attr new-value]]))

(defmethod intent2
  ::simple-toggle
  [db event _]
  [[:db.fn/call simple-toggle event]])

(defn ^:export simple-value [db {:keys [dat.view/entity dat.view/attr dat.view/value]}]
  (let [old-value (attr (d/entity db entity))]
    [[:db/retract entity attr old-value]
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
  (let [sim (or sim (:onyx.sim/selected-sim (d/entity db [:onyx/name :onyx.sim/settings])))]
    [[:db/add sim :onyx.sim/hidden-tasks task-names]]))

(defmethod intent2
  :onyx.sim.event/hide-tasks
  [db event input]
  [[:db.fn/call
    hide-tasks
    (assoc
      event
      :onyx.sim/task-names input)]])

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

#?(:cljs
(defn run-sim
  "DEPRECATED"
  [conn sim]
  ;; causes multi-ticking for all sims if one is running
  (let [running? (:onyx.sim/running? (d/entity @conn sim))]
    (when running?
      ;; FIXME: upgrade performance by doing everything inline
      (dispatch conn {:onyx/type :reagent/next-tick})
      (r/next-tick #(run-sim conn sim))))))

#?(:cljs
(defn run-sims [conn]
  (let [running? (d/q '[:find ?running .
                        :where
                        [?running :onyx.sim/running? true]]
                      @conn)]
    (when running?
      (d/transact! conn [[:db.fn/call db-onyx-tick]])
      (r/next-tick #(run-sims conn))))))

(defmethod intent
  :onyx.api/start
  [conn {:keys [:onyx.sim/sim]}]
  #?(:cljs
      (do
        (d/transact! conn [[:db/add sim :onyx.sim/running? true]])
        (run-sims conn))
      :clj (throw (ex-info "Cannot start simulator in clojure. Only implemented in cljs."))))

(defmethod intent
  :onyx.api/stop
  [db {:keys [:onyx.sim/sim]}]
  [[:db/add sim :onyx.sim/running? false]])

(defmethod intent
  :onyx.sim/toggle-play
  [conn {:keys [:onyx.sim/sim]}]
  (let [running? (:onyx.sim/running? (d/entity @conn sim))]
    (if running?
      (d/transact! conn [[:db/add sim :onyx.sim/running? false]])
      #?(:cljs
          (do
            (d/transact! conn [[:db/add sim :onyx.sim/running? true]])
            (run-sim conn sim))
          :clj (throw "Cannot start simulator in clojure. Only implemented in cljs.")))))

(defmethod intent
  :onyx.sim/select-view
  [db {:keys [selected]}]
  (if (keyword? selected)
    [[:db/add [:onyx/name :onyx.sim/settings] :onyx.sim/selected-view selected]]
    [{:db/id [:onyx/name :onyx.sim/settings]
      :onyx.sim/selected-sim selected
      :onyx.sim/selected-view :onyx.sim/sim-view}]))

(defmethod intent2
  :onyx.sim.event/select-view
  [db _ [selected]]
  (if (keyword? selected)
    [[:db/add [:onyx/name :onyx.sim/settings] :onyx.sim/selected-view selected]]
    [{:db/id [:onyx/name :onyx.sim/settings]
      :onyx.sim/selected-sim selected
      :onyx.sim/selected-view :onyx.sim/sim-view}]))

(defmethod intent2
  ::transitions
  [db {:as event :keys [onyx.sim/sim onyx.sim/transitions]} _]
  (let [sim (:db/id
              (or (d/entity db sim)
                (-> db
                    (d/entity [:onyx/name :onyx.sim/settings])
                    :onyx.sim/selected-sim)))
        tss (-> db
                (d/entity sim)
                :onyx.sim/transitions)]
    [[:db/retract sim :onyx.sim/transitions tss]
     [:db/add sim :onyx.sim/transitions (into tss transitions)]]))

#?(:cljs
(defmethod intent
  :onyx.sim.event/import-segments
  [conn {:keys [onyx.sim/sim onyx.sim/task-name]}]
  (let [uri (first (:onyx.sim/import-uris (d/entity @conn sim)))]
    (go (let [response (<! (http/get uri))]
          (log/info (str "retrieving edn from <" uri ">"))
          (log/debug "edn is...\n" (ppr-str (:body response)))
          (d/transact! conn [[:db.fn/call pull-and-transition-env sim
                               (for [seg (:body response)]
                                 #(onyx/new-segment % task-name seg))]]))))))

