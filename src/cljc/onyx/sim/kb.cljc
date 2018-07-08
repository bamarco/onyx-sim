(ns onyx.sim.kb)

;; TODO: transactor for kb

(defprotocol DB
  (-snap [db kb])
  (-transact! [db kb kbs dbs txs]))

(defmulti q
  (fn [kbs {:as q-expr ::keys [type]}]
    type))

(defmulti sub
  (fn [kb {:as q-expr ::keys [type]}]
    type))

(defn snap [kb]
  (into
    {}
    (for [[id db] kb]
      [id (if (satisfies? DB db) (-snap db kb) db)])))

; (defn transact! [kb fx]
;   (let [kbs (snap kb)
;         txses (fx kbs)]
;     (doseq [[id txs] txses]
;       (-transact! (get kb id) kb kbs (get kbs id) txs))))
