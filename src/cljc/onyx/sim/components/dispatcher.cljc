(ns onyx.sim.components.dispatcher
  (:require
    [com.stuartsierra.component :as component]
    [clojure.core.async :as async :refer [go go-loop <! >!]]
    [onyx.sim.console.event :as event]
    [taoensso.timbre :as log]))

(defn dispatch! [{:keys [event>]} intent & {:as event}]
  ;; ???: validate inputs?
  (go 
    (>! event> 
      (assoc (or event {}) :onyx.sim.event/intent intent))))

(defn kill! [{:keys [control>]}]
  (when control>
    (go (>! control> ::kill))))

(defn go-transactor! [event> knowbase]
  (let [control> (async/chan)]
    (go-loop []
      (let [[event ch] (async/alts! [control> event>])]
        (if (= ch control>)
          (if (= event ::kill)
            (log/info "Kill Signal recieved. Closing transactor...")
            (do 
              (log/warn "Unhandled signal:" event)
              (recur)))
          (do 
            (event/handle! knowbase event)
            (recur)))))
    control>))

(defrecord Dispatcher [event> control> knowbase]
  ;; ???: give tss> a buffer. May need to be flushed on stop.
  component/Lifecycle
  (start [component]
    (let [event> (or event> (async/chan))]
      (assoc
        component
        :event> event>
        :control> (or control> (go-transactor! event> knowbase)))))
  (stop [component]
    (kill! component)
    (assoc component
      :control> nil
      :knowbase nil
      :event> nil)))

(defn new-dispatcher
  ([] (new-dispatcher {}))
  ([opts] (map->Dispatcher opts)))
