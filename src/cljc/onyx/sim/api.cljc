(ns onyx.sim.api
  (:require [taoensso.timbre :as log]
            [onyx-local-rt.api :as onyx]
            [onyx-local-rt.impl :as i]
            [onyx.static.util :refer [kw->fn]]
            [onyx.plugin.protocols :as p]
            [datascript.core :as d]
            [clojure.spec.alpha :as s]
            [clojure.core.async.impl.protocols :refer [WritePort ReadPort Channel]]
            [clojure.core.async :as async :refer [go go-loop <! >!]]
            [onyx.sim.utils #?@(:clj  (:refer [xfn])
                                :cljs (:refer-macros [xfn]))]))
            

(defonce onyx-batch-size 20)

(def env-summary     onyx/env-summary)
(def tick            onyx/tick)
(def drain           onyx/drain)
; (def transition-env  onyx/transition-env)
(def new-segment     onyx/new-segment)

;;
;; Init
;;
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

(defn- init-plugin [task-state]
  (let [task (get-in task-state [:event :onyx.core/task-map])
        ns-name (str (name (:onyx/plugin task)))
        init-name (cond 
                    (= :input (:onyx/type task))
                    "input"
                    
                    (= :output (:onyx/type task))
                    "output")
        init (kw->fn (keyword ns-name init-name))]
    (assoc
      task-state
      :pipeline
      (init task))))

(defn- task-state><init []
  (map 
    (fn [[task-name task-state]]
      [task-name (if (plugin? task-state)
                    (init-plugin task-state)
                    task-state)])))

(defn- init-plugins [env]
  (update-in
    env
    [:tasks]
    #(into {} (task-state><init) %)))

(defn- recover-plugins! [env]
  (doseq [[task-name task] 
          (filter 
            (fn [[_ task]]
              (plugin? task))
            (:tasks env))]
    (p/recover! (:pipeline task) nil nil)))

(defn init [job]
  (let [env (-> job
              (onyx/init)
              (init-plugins))]
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

(defn- read-chan? [obj] 
  (satisfies? ReadPort obj))

(defn- go-batch 
  "from https://stackoverflow.com/questions/33620388/how-to-properly-batch-messages-with-core-async"
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
  "collect a vector of promise-chans"
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

(defn- go-transfer-pending-writes [env]
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
    ;; TODO: fix for async
   (go-loop [env env
             n n]
     (if (= n 0)
       env
       (let [env (<! (go-tick env))]
         (recur env (dec n))))))
  ([env]
   (go
     (let [this-action (:next-action env)
           _ (log/info "action" this-action)
           env (i/integrate-task-updates env this-action)
           _ (log/info "pending-writes" (:pending-writes env))
           env (if (empty? (:pending-writes env))
                  env
                  (<! (go-transfer-pending-writes env)))
          ;  _ (log/info "pending-writes" (:pending-writes env))
           env (i/transition-action-sequence env this-action)]
          ;  _ (log/info "next-action" (:next-action env))]
        env))))

(defn go-drain 
  "Asynchronously drains the env"
  [env & {:as opts :keys [max-ticks] :or {max-ticks 10000}}]
  ;; TODO: max-ticks
  (go-loop [env env]
    (log/info "go-loop :inbox/outbox " (get-in env [:tasks :in :inbox]) (get-in env [:tasks :out :outputs]))
    (if (onyx/drained? env)
      env
      (let [chan (go-tick env)
            env (<! chan)]
        (recur env)))))

(defn- batch! [pipeline event timeout]
  ;; TODO: batched
  (loop [segs []]
    (if-let [seg (p/poll! pipeline event timeout)]
      (recur (conj segs seg))
      segs)))

(defn poll-plugins! [env & {:as opts :keys [timeout] :or {timeout 1000}}]
  (into
    {}
    (comp
      (filter
        (fn [[_ task-state]]
          (input-plugin? task-state)))
      (map
        (fn [[task-name task-state]]
          [task-name
           (batch! (:pipeline task-state) (:event task-state) timeout)]))) 
    (:tasks env)))

(defn offer-plugins! [env])
  ;; TODO: return {output-task segments} that were taken by all output plugins

(defmulti transition-env 
  (fn [env action-data]
    (::event action-data)))

(defmethod transition-env ::init
  [_ {::keys [job]}]
  (init job))

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
  
(defmethod transition-env ::inputs
  [env {::keys [inputs]}]
  (new-inputs env inputs))

(defmethod transition-env ::poll!
  [env _]
  (new-inputs env
    (poll-plugins! env)))

(defmethod transition-env ::offer!
  [env action]
  (remove-outputs env
    (offer-plugins! env)))

; (defn into-meta [obj m]
;   (vary-meta
;    obj
;    #(into
;      (or % {})
;      m)))

; (defn inject-lifecycle-resources [job resources]
;   (update-in
;    job
;    [:onyx.core/lifecycles]
;    (fn [lifecycles]
;      (for [lc lifecycles]
;        (into-meta lc resources)))))

(defn simple-submit-job
  "Initializes the job and drains the job asynchronously."
  [job inputs & opts]
  (let [env (-> (init job)
                (new-inputs inputs))]
    (apply go-drain env opts)))

;;;
;;; !!!: New Hotness!
;;;
; (defn chan-poll-batch!
;   "Plugins that implement the Onyx Input poll! protocol can call this function to poll a batch of segments"
;   [chan batch-size]
;   (loop [segs []]
;     (if-not (< (count segs) batch-size) segs
;       (if-let [seg (async/poll! chan)]
;         (recur (conj segs seg))
;         segs))))

; #?
; (:clj
;   (defn poll-env!
;     "Reads a batch of segments for every input plugin for the given onyx env and creates a ::batch transition."
;     [env & {:as opts :keys [timeout]}]
;     {::event   ::inputs
;       ::onyx-id (::tenancy-id env)
;       ::inputs 
;       (into {})
;       (for [[task-name {:as task :keys [pipeline event]}] (filter (fn [[_ task]] (input-task? task)) (:tasks env))]
;         (let [batch (p/poll! pipeline event timeout)]
;           [task-name batch]))}))

; #?
; (:clj
;   (defn flush-env! 
;     "Writes a batch of segments for every output plugin for the given output env and creates a ::flush transition"
;     [env]
;     (let [replica   nil
;           messenger nil]
;       {::event   ::flush}
;       ::onyx-id (::tenancy-id env)
;       ::remove
;       (into {}
;         (for [[task-name {:as task :keys [pipeline event outbox]}] (filter (fn [[_ task]] (output-task? task)) (:tasks env))]
;           ;; TODO: handle prepare-batch and write-batch delays
;           (do
;             (p/prepare-batch pipeline event replica messenger)
;             (p/write-batch pipeline event replica messenger)
;             [task-name outbox]))))))

; #?
; (:cljs 
;   (defn active-envs [{:as simulator :keys [conn envs]}]
;       (let [active-ids (d/q) 
;                       '[:find ?tenancy-id .
;                         :where
;                         [?job ::active?    true]
;                         [?job ::tenancy-id ?tenancy-id]]
;                       @conn])
;       (select-keys @envs active-ids)))

; #?
; (:cljs
;   (defn go!
;     "Starts a go-loop that will loop through all the envs in an onyx simulator. Polling the input plugins for a batch of segments, draining the envs, and writing the batch of output segments to the output plugins"
;     [{:as simulator :keys [conn]} & opts]
;     ;; ???: add throttling when no segments are in flight? might churn cpu or maybe core.async has it covered
;     (go-loop []
;       (let [actives (active-envs simulator)]
;         (doseq [env actives]
;           (let [batch (apply poll-env! env opts)]
;             (d/transact! conn batch)))
;         (doseq [env actives]
;           (let [env (<! (apply go-drain env opts))
;                 batch (apply flush-env! env opts)]
;             (d/transact! conn batch))))  
;       (recur))))

;; ???: side-effect functions return events rather than voids? why not just use transactor model?

; (defn submit-job! 
;   "Transacts a job into the simulator."
;   [{:as sim :keys [conn]} job]
;   (let [job-id   (or (:job-id job)) ;(gen-uuid))
;         job-hash (hash job)
;         tenancy-id (or (:tenancy-id job) job-hash)]
;     (d/transact! 
;       conn
;       ;; ???: which of this identifiers are neccessary: tenancy-id, job-id, job-hash?
;       {::event      :onyx.sim.api/init
;        ::tenancy-id tenancy-id
;        ::job-id     job-id
;        ::job-hash   job-hash
;        ::job        job})))
