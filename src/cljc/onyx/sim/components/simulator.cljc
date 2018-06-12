;(ns onyx.sim.components.simulator
;   (:require 
;     [com.stuartsierra.component :as component]
;     [taoensso.timbre :as log]
;     [datascript.core :as d]
;     [onyx.sim.kb :as kb]
;     #?(:cljs [posh.reagent :as posh])
;     #?(:cljs [onyx.sim.ui :as ui])
;     #?(:cljs [reagent.core :as r])
;     [clojure.core.async :as async :refer [go go-loop <! >!]]
;     [onyx.sim.api :as api]))

; (defn gen-uuid []
;   (d/squuid))

; (defn- simplify [sim-job]
;   (-> sim-job
;       (clojure.set/rename-keys {:onyx.core/catalog         :catalog
;                                 :onyx.core/workflow        :workflow
;                                 :onyx.core/lifecycles      :lifecycles
;                                 :onyx.core/windows         :windows
;                                 :onyx.core/triggers        :triggers
;                                 :onyx.core/task-scheduler  :task-scheduler
;                                 :onyc.core/metadata        :metadata
;                                 :onyx.core/flow-conditions :flow-conditions})
;       (select-keys [:catalog :workflow :lifecycles :windows :triggers :task-scheduler :metadata :flow-conditions])))

; (defn into-envs [envs-before envs-after]
;   (into
;     envs-before
;     (for [env envs-after]
;       [(:onyx/job-id env) env])))

; (def ?envs
;   '{:onyx.sim.kb/type :onyx.sim.kb.ratom/cursor
;     :onyx.sim.kb/in {$ :state}
;     :onyx.sim.kb.ratom/path [:envs]})

; (defn go-schedule-jobs! [{:keys [knowbase control>]}]
;   ;; TODO: scheduler
;   (go-loop []
;     (let [snapshot (kb/snap knowbase)
;           envs (kb/q snapshot ?envs)
;           envs-before (vec (remove api/completed? (vals envs)))
;           _ (log/debug "Scheduling jobs:" (mapv :onyx/job-id envs-before))
;           [envs-or-signal ch] (async/alts! [(api/go-envs! envs-before) control>])]
;       (if (= ch control>)
;         (if (= envs-or-signal ::kill)
;           (log/info "Kill Signal recieved. Closing job scheduler...")
;           (do 
;             (log/warn "Unhandled signal:" envs-or-signal)
;             (recur)))
;         (let [pause? (every?
;                        #(= (:onyx.sim.api/signal %) :onyx.sim.api/pause)
;                         envs-or-signal)
;               envs-after (map api/mark-completed envs-or-signal)]
;           (if pause?
;             (do
;               (log/debug "Nothing to poll, pausing now")
;               (<! (async/timeout 100))
;               (kb/transact!
;                 knowbase
;                 (fn [kbs]
;                   {:state [[:update :envs into-envs (map api/clear-signals envs-after)]]})))
;             (do
;               (log/debug "Envs processed successfully")
;               (kb/transact!
;                 knowbase
;                 (fn [kbs]
;                   {:state [[:update :envs into-envs envs-after]]}))))
;           (recur))))))

; (defn submit-job [{:keys [knowbase]} job]
;   (let [job (update job :onyx/job-id #(or % (gen-uuid)))
;         job-id (:onyx/job-id job)
;         env (assoc (api/init (simplify job)) :onyx/job-id job-id)]
;     ;; TODO: make idempotent based on job-id/job-hash
;     (log/info "Submitting job:" (:onyx/job-id job))
;     (kb/transact!
;       knowbase
;       (fn [kbs]
;         {:state [[:assoc-in [:envs job-id] env]]
;          :db [job]}))
;     true))

; (defrecord OnyxSimulator [knowbase control> event>]
;   component/Lifecycle
;   (start [component]
;     (let [sim
;           (assoc
;             component
;             :control> (or control> (async/chan))
;             :event>   (or event> (async/chan)))]
;       ;; FIXME: make idempotent
;       (go-schedule-jobs! sim)
;       sim))
;   (stop [component]
;     (go (>! control> ::kill))
;     (assoc component
;       :control> nil
;       :event> nil)))

; (defn new-simulator
;   ([]
;    (new-simulator {}))
;   ([opts]
;    (map->OnyxSimulator opts)))
