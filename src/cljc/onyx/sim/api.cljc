(ns onyx.sim.api
  (:require 
    [taoensso.timbre :as log]
    [com.stuartsierra.component :as component]
    [onyx-local-rt.api :as onyx]
    [onyx-local-rt.impl :as i]
    [reagent.core :as r]
    [onyx.sim.utils :refer [read-chan? <apply]]
    [onyx.static.util :refer [kw->fn]]
    [onyx.plugin.protocols :as p]
    [onyx.plugin.core-async]
    [datascript.core :as d]
    [clojure.spec.alpha :as s]
    [onyx.sim.log.zookeeper :as zlog]
    [onyx.extensions :as extensions]
    [clojure.core.async :as async :refer [go go-loop <! >!]]))
  ; #?(:cljs
  ;    (:require-macros [onyx.sim.api :refer [<transition-env]])))

(defonce onyx-batch-size 20)

(def env-summary     onyx/env-summary)
(def tick            onyx/tick)
(def drain           onyx/drain)
(def new-segment     onyx/new-segment)
(def drained? onyx/drained?)

(def schema-idents
  [
    {:db/ident :onyx.core/catalog
      :db/valueType :db.type/ref
      :db/cardinality :db.cardinality/many}
    {:db/ident :onyx.core/flow-conditions
      :db/valueType :db.type/ref
      :db/cardinality :db.cardinality/many}
    {:db/ident :onyx/job-id
      :db/unique :db.unique/identity}
    {:db/ident ::catalog-id
      :db/unique :db.unique/identity}
    {:db/ident :onyx.core/job
     :db/valueType :db.type/ref}
    {:db/ident ::env
      :db/valueType :db.type/ref}])

(defn idents->schema [idents]
  (into
    {}
    (map (fn [{:as id-entity :db/keys [ident]}]
            [ident id-entity]))
    idents))

;;
;; Init
;;
(defn- simplify-job [sim-job]
  (-> sim-job
      (clojure.set/rename-keys {:onyx.core/catalog         :catalog
                                :onyx.core/workflow        :workflow
                                :onyx.core/lifecycles      :lifecycles
                                :onyx.core/windows         :windows
                                :onyx.core/triggers        :triggers
                                :onyx.core/task-scheduler  :task-scheduler
                                :onyc.core/metadata        :metadata
                                :onyx.core/flow-conditions :flow-conditions})
      (select-keys [:catalog :workflow :lifecycles :windows :triggers :task-scheduler :metadata :flow-conditions])))

(defn- fingerprint-env [env job]
  ; (log/info "env fingerprint" [(::catalog-id job) (:onyx/job-id job)])
  (into
    (assoc env ::master (::catalog-id job))
    (select-keys job [:onyx/job-id :onyx/doc :onyx.sim/label])))

(defn- plugin? [task-state]
  (get-in task-state [:event :onyx.core/task-map :onyx/plugin]))

(defn- input-plugin? [task-state]
  (and 
    (plugin? task-state)
    (= :input (get-in task-state [:event :onyx.core/task-map :onyx/type]))))

(defn- output-plugin? [task-state]
  (and
    (plugin? task-state)
    (= :output (get-in task-state [:event :onyx.core/task-map :onyx/type]))))

(defn- lc-hack 
  "DEPRECATED This is a temporary lifecycle hack. Don't use this outside of onyx-sim. Eventually needs to be replaced with full onyx replica state machine and replica support."
  [event]
  (let [f (get-in event [:onyx.core/compiled :compiled-before-task-start-fn])]
    (merge event (f event))))

(defn- plugin-kw [{:onyx/keys [plugin type]}]
  (if (simple-keyword? plugin)
    (keyword
      (-> plugin name str)
      (cond
        (= :input type)
        "input"
        
        (= :output type)
        "output"))
    plugin))

(defn- init-plugin [task-state]
  ; (log/debug "evvvvvvv" 
  ;   (get-in task-state [:event :onyx.core/task-map :onyx/name]) 
  ;   (:core.async/chan (lc-hack (:event task-state)))
  ;   (:seq/seq (lc-hack (:event task-state))))
  (let [task (get-in task-state [:event :onyx.core/task-map])
        hacked-event (lc-hack (:event task-state))
        init (kw->fn (plugin-kw task))]
    (assoc
      task-state
      :pipeline
      (init hacked-event))))

(defn- task-state><init []
  (map 
    (fn [[task-name task-state]]
      [task-name (if (plugin? task-state)
                    (init-plugin task-state)
                    task-state)])))

(defn- init-plugins [env]
  (update
    env
    :tasks
    #(into {} (task-state><init) %)))

(defn- recover-plugins! [env]
  (doseq [[task-name task] 
          (filter 
            (fn [[_ task]]
              (plugin? task))
            (:tasks env))]
    (p/recover! (:pipeline task) nil nil)))

(defn init [job]
  ; (log/info "initting" job)
  (let [env (-> job
              (simplify-job)
              (onyx/init)
              (fingerprint-env job)
              (init-plugins))]
    ; (log/debug "initted?" (get-in env [:tasks :in :pipeline]))
    (recover-plugins! env)
    env))

;;
;; segments
;;
(defn new-inputs 
  "Adds the {input-task segments} to the inbox of the given tasks"
  [env inputs]
  (reduce
    (fn [env [task-name segments]]
      (update-in
        env
        [:tasks task-name :inbox]
        into
        segments))
    env
    inputs))

(defn remove-outputs 
  "Removes the {output-task segments} from the outputs of the given tasks"
  [env outputs]
  (reduce
    (fn [env [task-name segments]]
      (update-in
        env
        [:tasks task-name :outputs]
        (fn [outbox]
          ;; ???: actually check for equality rather than count? diff?
          (into [] (drop (count segments)) outbox))))
    env
    outputs))

(defn step
  "Ticks for one full lifecycle."
  [env]
  (reduce
    (fn [env _]
      (if (= (:next-action env) :lifecycle/after-batch)
        (reduced (onyx/tick env))
        (onyx/tick env)))
    (onyx/tick env)
    (range 1000)))

(defn- pending-outputs? [task-state]
  (not (empty? (:outputs task-state))))

(defn- flushable? [env]
  (some
    pending-outputs?
    (eduction
      (comp
        (map second)
        (filter output-plugin?))
      (:tasks env))))

(defn out
  "Returns {output-task output-segments} of onyx env presumably after draining."
  [env]
  (into
    {}
    (comp
      (filter 
        (fn [[_ task]] 
          (= (get-in task [:event :onyx.core/task-map :onyx/type]) :output)))
      (map 
        (fn [[task-name task]]
          [task-name (:outputs task)])))
    (:tasks env)))

(defn- go-batch 
  "borrows from https://stackoverflow.com/questions/33620388/how-to-properly-batch-messages-with-core-async"
  [in out max-time max-count]
  (let [lim-1 (dec max-count)]
    (async/go-loop [buf [] t (async/timeout max-time)]
      (let [[v p] (async/alts! [in t])]
        (cond
          (= p t)
          (do
            (async/>! out buf)
            (recur [] (async/timeout max-time)))

          (nil? v)
          (if (seq buf)
            (async/>! out buf))

          (== (count buf) lim-1)
          (do
            (async/>! out (conj buf v))
            (recur [] (async/timeout max-time)))

          :else
          (recur (conj buf v) t))))))

(defn- go-collect
  "collect a vector of promise-chans into a single chan, preserving order"
  [in-chans]
  (go-loop [ins in-chans
            segs []]
    (if (empty? ins)
      segs
      (let [seg-or-chan (first ins)
            seg (if (read-chan? seg-or-chan)
                  (<! seg-or-chan)
                  seg-or-chan)]
        (recur (rest ins) (conj segs seg))))))

(defn- go-transfer-pending-writes 
  "Asynchronously collects segments from :pending-writes"
  [env]
  (go-loop [env env
            writes (:pending-writes env)]
    (if (empty? writes)
      (assoc env :pending-writes {})
      (recur 
        (let [[task-name segments] (first writes)
              segments (<! (go-collect segments))]
          (update-in env [:tasks task-name :inbox] into segments))
        (rest writes)))))

(defn go-tick
  "Asynchronously ticks the env."
  ([env n]
   (go-loop [env env
             n n]
     (if (= n 0)
       env
       (let [env (<! (go-tick env))]
         (recur env (dec n))))))
  ([env]
   (go
     (let [this-action (:next-action env)
          ;  _ (log/debug "action" this-action)
           env (i/integrate-task-updates env this-action)
          ;  _ (log/debug "pending-writes" (:pending-writes env))
           env (if (empty? (:pending-writes env))
                  env
                  (<! (go-transfer-pending-writes env)))
          ;  _ (log/debug "pending-writes" (:pending-writes env))
           env (i/transition-action-sequence env this-action)]
          ;  _ (log/debug "next-action" (:next-action env))]
        env))))

(defn go-drain 
  "Asynchronously drains the env"
  [env & {:as opts :keys [max-ticks] :or {max-ticks 10000}}]
  ;; TODO: max-ticks
  (go-loop [env env
            ticks-left max-ticks]
    ; (log/debug "go-loop :inbox/outbox " (get-in env [:tasks :in :inbox]) (get-in env [:tasks :out :outputs]))
    (cond
      (onyx/drained? env)
      env

      (= ticks-left 0)
      (throw 
        (ex-info
          (str "Ticked " max-ticks " times and never drained, runtime will not proceed with further execution.")
          {}))

      :else
      (let [chan (go-tick env)
            env (<! chan)]
        (recur env (dec ticks-left))))))

(defn- batch-in! [task-state timeout]
  (loop [segs []
         batch-left (get-in task-state [:event :onyx.core/task-map :onyx/batch-size])]
    ; (log/info "batch-left" batch-left)
    (if (= batch-left 0)
      (do 
        ; (log/info "Batch full for" (get-in task-state [:event :onyx.core/task-map :onyx/name]))
        ; (log/info "  segs:" segs)
        segs)
      (if-let [seg (p/poll! (:pipeline task-state) (lc-hack (:event task-state)) timeout)]
        (recur (conj segs seg) (dec batch-left))
        (do
          ; (log/info "Batch ready for" (get-in task-state [:event :onyx.core/task-map :onyx/name]))
          ; (log/info "  segs:" segs)
          segs)))))

(defn- go-write-batch! [task-state & {:as opts :keys [timeout max-attempts] :or {timeout 1000 max-attempts 10000}}]
  (let [event (assoc (lc-hack (:event task-state)) :onyx.core/write-batch (:outputs task-state))]
    ; (log/debug "prepared before"  @(:prepared (:pipeline task-state)))
    (p/prepare-batch (:pipeline task-state) event nil nil)
    ; (log/debug "prepared after"  @(:prepared (:pipeline task-state)))
    (go-loop [max-attempts max-attempts]
      (let [finished? (p/write-batch (:pipeline task-state) event nil nil)]
        (if finished?
          true
          (if (= max-attempts 0)
            false
            (do
              (<! (async/timeout timeout))
              (recur (dec max-attempts)))))))))

(defn poll-plugins! [env & {:as opts :keys [timeout] :or {timeout 1000}}]
  ; (log/info "poll-plugins!" (:tasks env))
  (into
    {}
    (comp
      (filter
        (fn [[_ task-state]]
          (input-plugin? task-state)))
      (map
        (fn [[task-name task-state]]
          [task-name
           (batch-in! task-state timeout)]))) 
    (:tasks env)))

(defn go-write-plugins! [env & {:as opts :keys [timeout] :or {timeout 1000}}]
  ;; ???: park if plugins not ready?
  (go-loop [env env
            outs (filter
                   (fn [[_ task-state]]
                    (output-plugin? task-state)) 
                   (get-in env [:tasks]))]
    (if (empty? outs)
      env
      (let [[task-name task-state] (first outs)]
        (if (<! (go-write-batch! task-state))
           (recur (assoc-in env [:tasks task-name :outputs] []) (rest outs))
           false))))) ;; ???: false as error?

(defn go-poll-plugins! [env]
  (let [inputs (poll-plugins! env)]
    ; (log/info "new inputs" inputs)
    (new-inputs env inputs)))

(defn polls-closed? [env]
  ; (log/info "Polls closed?" 
  ;   (into []
  ;     (comp
  ;       (map second)
  ;       (filter input-plugin?)
  ;       (map :pipeline)
  ;       (map p/completed?))
  ;     (:tasks env)))
  (empty?
    (into
      []
      (comp
        (map second)
        (filter input-plugin?)
        (map :pipeline)
        (remove p/completed?))
      (:tasks env))))

(defmulti transition-env
  (fn [env action-data]
    (let [tss-key (if (keyword? action-data)
                    action-data
                    (or (::event action-data) (:event action-data)))]
      ; (log/info "transition-env event:" (or tss-key :default))
      tss-key)))

; #?
; (:clj
;   (defmacro <transition-env [env action-data]
;     (<apply env action-data)))
    ; `(let [maybe-chan# (transition-env ~env ~action-data)]
    ;    (if (read-chan? maybe-chan#)
    ;      (<! maybe-chan#)
    ;      maybe-chan#))))

(defn go-transitions [env transitions]
  (go-loop [env env
            transitions transitions]
    ; (log/info "tss" (first transitions))
    (if (empty? transitions)
      env
      (let [env (<apply transition-env env (first transitions))]
        (recur env (rest transitions))))))

(defn clear-signals [env]
  (dissoc env ::signal))

(defn mark-completed [{:as env ::keys [signal]}]
  (if (= ::complete signal)
    (assoc env ::completed? true)
    env))

(defn completed? [{:as env ::keys [completed?]}]
  completed?)

(defn go-env! [env]
  (go
    (if (polls-closed? env)
      (do
        (log/debug "Polls closed completing job...")
        (assoc env ::signal ::complete))
      (let [inputs (poll-plugins! env)]
        (if (empty? inputs)
          (assoc env ::signal ::pause)
          (<!
            (go-transitions
              env
              [{::event ::new-inputs
                ::inputs inputs}
               ::go-drain
               ::go-write!])))))))

(defn go-cycle [env & {:as opts :keys [break-point] :or {break-point :lifecycle/read-batch}}]
  (go-loop [env env]
    (let [env (<apply transition-env env ::go-tick)]
      ; (log/info "cycle next-action" (:next-action env) (drained? env))
      (if (= (:next-action env) break-point)
        env
        (recur env)))))

(defn status [env]
  (cond
    (not (drained? env)) ::in-flight
    (flushable? env) ::flush-ready
    (polls-closed? env) ::polls-closed
    :else ::polls-ready))

(defn go-step
  ""
  [env]
  (let [status (status env)]
    ; (log/info "go-step status" status)
    (case status
      ::in-flight (go-cycle env)
      ::flush-ready (go-write-plugins! env)
      ::polls-ready (go-poll-plugins! env)
      env)))

(defn go-job!
  "Initializes the job and drains the job asynchronously"
  [job & opts]
  (go-loop [env (init job)]
    (let [{:as env ::keys [signal]} (<! (go-env! env))]
      (case signal 
        ::complete
        env
        ::pause
        (do
          (async/timeout 100)
          (recur env))
        (recur env)))))

(defn go-envs! [envs]
  (go-loop [envs-before envs
            envs-after []]
    (if (empty? envs-before)
      envs-after
      (let [env (<! (go-env! (first envs-before)))]
        (recur (rest envs-before) (conj envs-after env))))))


;;;
;;; Transitions
;;;
(defmethod transition-env ::init
  [env {::keys [job]}]
  (or env (init job)))

(defmethod transition-env ::tick
  [env {::keys [times]}]
  (if times
    (tick env times)
    (tick env)))

(defmethod transition-env ::go-tick
  [env {::keys [times]}]
  (if times
    (go-tick env times)
    (go-tick env)))

(defmethod transition-env ::step
  [env _]
  (step env))

(defmethod transition-env ::drain
  [env _]
  (drain env))
  
(defmethod transition-env ::new-inputs
  [env {::keys [inputs]}]
  (new-inputs env inputs))

(defmethod transition-env ::poll!
  [env _]
  (new-inputs env
    (poll-plugins! env)))

(defmethod transition-env ::go-drain
  [env action]
  (go-drain env))

(defmethod transition-env ::go-step
  [env action]
  (go-step env))

(defmethod transition-env ::go-cycle
  [env action]
  (go-step env))

(defmethod transition-env ::go-write!
  [env action]
  (go-write-plugins! env))

(defmethod transition-env :default
  [env action]
  ;; treats unknown transitions as noop
  (if (nil? env)
    ;; ???: if env is nil action must be an env
    action
    (do
      (log/warn "Treating transition " action " as a noop")
      env)))