(ns onyx.sim.components.sim
  (:require
    [com.stuartsierra.component :as component]
    [taoensso.timbre :as log]
    [onyx.sim.api :as api]
    [datascript.core :as d]
    [onyx.sim.utils :refer [gen-uuid <apply]]
    [clojure.core.async :as async :refer [go go-loop <! >!]]
    #?(:cljs [reagent.core :refer [atom]])))

(defn kill-warn [control>]
  (fn [[sig ch]]
    (when (and (= ch control>) (= sig :kill))
      (log/info "Kill Signal received."))))

; (defn alt-loop! [{:as chans :keys [control>]}]
;   (let [control> (or control> (async/chan))]
;     (let [[seg ch] (async/alts! chans)]
;       (condp
;         (fn [[seg ch] [ch-guard kw-guard]]
;           (if (keyword? seg)
;             (and (= kw-guard seg) (= ch ch-cguard))
;             (and (= kw-guard (guard-type seg) (= ch ch-guard)))))
;         [seg ch]))
;     control>))

(defn- controlled-loop [control> the-loop]
  (let [control> (or control> (async/chan))]
    (do the-loop)
    control>))

(defn fire-transition? [now {:as tss :keys [last-fired frequency]}]
  (= now (+ last-fired frequency)))

(defn min-timeout [tsses]
  (let [now nil]
    (transduce
      (map 
        (fn [{:keys [frequency last-fired]}]
          (- frequency (- now last-fired))))
      min
      tsses)))

; (defn go-envs2! [envs-atom recurring-tsses tss> & {:as opts :keys [control>]}]
;   (controlled-loop control>
;     (go-loop []
;       (let [now nil
;             timeout> (async/timeout (min-timeout recurring-tsses))
;             [seg ch] (async/alts! [control> tss> timeout>])
;             tss! (fn [tss]
;                    (let [env-id (:onyx/job-id tss)
;                          env (get @envs-atom env-id)
;                          env-after (<apply api/transition-env env tss)]
;                     ;; TODO: if recurring tss update last-fired time
;                     (when-not (compare-and-set! envs-atom envs (assoc envs env-id env-after))
;                       ;; FIXME: Not quite correct on the exception logic. Adding new envs should not be a problem.
;                       (throw (ex-info "The envs-atom should only be set from the go-envs! loop. Race conditions may occur. Failed to process tss." {:tss tss})))))]
;         (condp = ch
;           control>
;           (case seg
;             ::kill (log/info "Kill Signal received. Closing job scheduler...")
;             (do (log/warn "Unhandled signal:" seg) (recur)))

;           tss>
;           (do (tss! seg) (recur))

;           timeout>
;           (let [now nil]
;             (doseq [tss (filter (partial fire-transition? now) recurring-tsses)]
;               (tss! tss))
;             (recur)))))))

(defn go-envs! [envs tss> & {:as opts :keys [control>]}]
  ; (log/info "go-envs!")
  (let [control> (or control> (async/chan))]
    (go-loop []
      (let [[tss-or-sig ch] (async/alts! [control> tss>])]
        ; (log/info "received tss" (:onyx.sim.api/event tss-or-sig))
        (if (= ch control>)
          (if (= tss-or-sig ::kill)
            (log/info "Kill Signal recieved. Closing job scheduler...")
            (do 
              (log/warn "Unhandled signal:" tss-or-sig)
              (recur)))
          (let [{:as tss :onyx/keys [job-id] :onyx.sim.api/keys [event]} tss-or-sig
                _ (log/info "tssing" job-id)
                env (get @envs job-id)
                ; _ (log/info "env retrieved" (get-in env [:tasks :in :inbox]) (get-in env [:tasks :out :outputs]))
                env-after (<apply api/transition-env env tss)]
            (swap! envs assoc job-id env-after)
            (when (and (= event :onyx.sim.api/go-step)
                       (not= (api/status env-after) :onyx.sim.api/polls-closed))
              ;; FIXME: use different marker for continuous transitions
              (go (>! tss> tss)))
              
            (recur)))))
    control>))

(defn submit-job [{:keys [tss>]} job]
  (let [job-id (or (:onyx/job-id job) (gen-uuid))]
    (log/info "Submitting job #" job-id)
    (go (>! tss> {:onyx.sim.api/event :onyx.sim.api/init
                  :onyx/job-id job-id
                  :onyx.sim.api/job (assoc job :onyx/job-id job-id)}))))

(defn transition! [{:as sim :keys [tss>]} tss]
  (go (>! tss> tss)))

(defn kill! [{:keys [control>]}]
  (when control>
    (go (>! control> ::kill))))

(defrecord OnyxSimulator [envs tss> control>]
  ;; ???: give tss> a buffer. May need to be flushed on stop.
  component/Lifecycle
  (start [component]
    (let [tss> (or tss> (async/chan))
          envs (or envs (atom {}))]
      (assoc
        component
        :tss> tss>
        :envs envs
        :control> (or control> (go-envs! envs tss>)))))
  (stop [component]
    (kill! component)
    (assoc component
      :control> nil
      :envs nil
      :tss> nil)))

(defn new-sim
  ([]
   (new-sim {}))
  ([opts]
   (map->OnyxSimulator opts)))
