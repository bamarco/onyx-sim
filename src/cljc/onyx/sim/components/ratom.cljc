(ns onyx.sim.components.ratom
  (:require
    [com.stuartsierra.component :as component]
    [taoensso.timbre :as log]
    [onyx.sim.substitutions :refer [substitute extract-db extract-inputs]]
    #?(:cljs [reagent.core :as r :refer [atom]])
    #?(:cljs [reagent.ratom :as ratom])
    [onyx.sim.kb :as kb]))

(defmulti conj-tx
  (fn [dbs tx]
    (first tx)))

(defmethod conj-tx :assoc
  [dbs [_ k v]]
  (assoc dbs k v))

(defmethod conj-tx :assoc-in
  [dbs [_ path v]]
  (assoc-in dbs path v))

(defmethod conj-tx :update
  [dbs [_ k f & args]]
  (apply update dbs k f args))

(defmethod conj-tx :update-in
  [dbs [_ path f & args]]
  (apply update-in dbs path f args))

(defmethod conj-tx :reset
  [dbs [_ state]]
  state)

(defmethod conj-tx :default
  [kbs tx]
  (log/warn "Unknown tx type skipping tx:" tx)
  kbs)

(defmethod kb/q :onyx.sim.kb.ratom/cursor
  [kbs {:as q-expr :onyx.sim.kb.ratom/keys [in path]} & {:as lk}]
  (let [inputs (extract-inputs kbs q-expr lk)
        path (substitute path inputs)
        state (extract-db kbs q-expr)]
    (get-in state path)))

(defmethod kb/sub :onyx.sim.kb.ratom/cursor
  [kb {:as q-expr :onyx.sim.kb.ratom/keys [in path]} & {:as lk}]
  (let [inputs (extract-inputs kb q-expr lk)
        path (substitute path inputs)
        ratom (:ratom (extract-db kb q-expr))]
    #?(:cljs (ratom/cursor ratom path)
       :clj (let [p (promise)]
              (deliver p (get-in @ratom path))
              p))))

(defrecord Ratom [ratom]
  component/Lifecycle
  (start [component]
    (assoc component
      :ratom (or ratom (atom {}))))
  (stop [component]
    (assoc
      component
      :ratom nil))
  kb/DB
  (-snap [_ kb]
    @ratom))
  ; (-transact! [_ kb kbs dbs txs]
  ;   (reset! ratom
  ;     (reduce
  ;       conj-tx
  ;       dbs
  ;       txs))))

(defn new-ratom
  ([] (new-ratom {}))
  ([opts] (map->Ratom opts)))
