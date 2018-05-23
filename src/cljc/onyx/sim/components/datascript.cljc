(ns onyx.sim.components.datascript
  (:require
    [taoensso.timbre :as log]
    [com.stuartsierra.component :as component]
    [onyx.sim.kb :as kb]
    [com.rpl.specter :as specter]
    #?(:cljs [posh.reagent :as posh])
    #?(:cljs [reagent.ratom :refer [make-reaction]])
    [onyx.sim.api :as api]
    #?(:cljs [onyx.sim.ui :as ui])
    [datascript.core :as d]))

(defn- substitute [form inputs]
  ; (log/info "subst" form inputs)
  (specter/transform
    (specter/walker symbol?)
    (fn [sym]
      ; (log/info "checking " sym " for replace " (get inputs sym))
      (if-let [replacement (get inputs sym)]
        replacement
        sym))
    form))

; (defn pull [pull-fn db* {:as q-expr :onyx.sim.kb.pull/keys [expr in eid]} & args]
;   (let [local-knowledge (first args)
;         db-id (get in '$)
;         db (get kb* db-id)
;         knowledge (merge kb* local-knowledge)
;         expr (substitute knowledge in expr)
;         eid (substitute knowledge in eid)]
;     (pull-fn db expr eid)))

; (defn q [q-fn db* {:as q-expr :onyx.sim.kb.q/keys [find in where]} & args]
;   (log/info "qing" find in where)
;   (let [local-knowledge (first args)
;         _ (log/info "lk" local-knowledge)
;         knowledge (merge kb* local-knowledge)
;         _ (log/info "kn" knowledge)
;         db-id (get in '$)
;         db (get-in kb* [db-id :conn])
;         input-keys (keys (dissoc in '$))
;         _ (log/info "ik" (get knowledge (get in '$)))
;         ins (map #(get knowledge (get in %)) input-keys)]
;     (log/info "ins" ins)
;     (log/info "db-id" db-id)
;     (log/info "db" db)
;     (log/info "ins" ins)
;     (apply q-fn {:find find :in ins :where where} db ins)))

(defn- extract-db [kb* {:as q-expr :onyx.sim.kb.datascript/keys [in]}]
  (get kb* (get in '$)))

(defn- extract-conn [kb q-expr]
  (:conn (extract-db kb q-expr)))

(defn- extract-inputs [kb* {:as q-expr :onyx.sim.kb.datascript/keys [in]} local-knowledge]
  ; (log/info "xtract")
  ; (log/info "xtr-in" in)
  ; (log/info "xlocal" local-knowledge)
  (let [knowledge (merge kb* local-knowledge)]
    (into {}
      (for [[sym k] in]
        [sym (get knowledge k)]))))

(defn- q-expr->ds-q-expr [{:as q-expr :onyx.sim.kb.datascript/keys [find in where]}]
  {:find find
   :in (into ['$] (keys (dissoc in '$)))
   :where where})

(defn- q-expr->ds-pull [{:as q-expr :onyx.sim.kb.datascript/keys [pull-expr eid in]} inputs]
  [(substitute pull-expr inputs) (substitute eid inputs)])

(defmethod kb/q :onyx.sim.kb.datascript/pull
  [kbs q-expr & {:as lk}]
  (let [db (extract-db kbs q-expr)
        inputs (extract-inputs kbs q-expr (first lk))
        [pull-expr eid] (q-expr->ds-pull q-expr inputs)]
    (d/pull db pull-expr eid)))

(defmethod kb/sub :onyx.sim.kb.datascript/pull
  [kb q-expr & {:as lk}]
  ; (log/info "kpull")
  (let [conn (extract-conn kb q-expr)
        ; _ (log/info "kpconn" conn)
        inputs (extract-inputs kb q-expr lk)
        ; _ (log/info "kpinputs" inputs)
        [pull-expr eid] (q-expr->ds-pull q-expr inputs)
        ; _ (log/info "k e e" pull-expr eid)
        reaction #?(:cljs
                    (posh/pull conn pull-expr eid)
                    :clj
                    (let [p (promise)]
                      (deliver (d/pull conn pull-expr eid))
                      p))]
    reaction))

(defmethod kb/q :onyx.sim.kb.datascript/q
  [kbs q-expr & {:as lk}]
  (let [db (extract-db kbs q-expr)
        inputs (extract-inputs kbs q-expr lk)
        ds-q-expr (q-expr->ds-q-expr q-expr)
        ins (map inputs (rest (:in ds-q-expr)))]
    (apply d/q q-expr db ins)))

(defmethod kb/sub :onyx.sim.kb.datascript/q
  [kb q-expr & {:as lk}]
  ; (log/info "ksub")
  (let [conn (extract-conn kb q-expr)
        ; _ (log/info "kconn" conn)
        inputs (extract-inputs kb q-expr lk)
        ; _ (log/info "kinputs" inputs)
        ds-q-expr (q-expr->ds-q-expr q-expr)
        ; _ (log/info "kexpr" ds-q-expr)
        ins (map inputs (rest (:in ds-q-expr)))
        ; _ (log/info "kins" ins)
        reaction #?(:cljs
                    (make-reaction
                      (fn [] 
                        (apply d/q ds-q-expr @conn ins)))
                    :clj
                    (let [p (promise)]
                      (deliver (apply d/q ds-q-expr @conn ins))
                      p))]
    reaction))

(defrecord Datascript [conn]
  component/Lifecycle
  (start [component]
    ;; TODO: move idents to edn
    (let [schema (api/idents->schema (concat api/schema-idents #?(:cljs ui/schema-idents :clj nil)))
          conn (or conn (d/create-conn schema))]
      #?(:cljs (posh/posh! conn))
      (assoc component
        :conn conn)))
  (stop [component]
    (assoc component
      :conn nil))
  kb/DB
  (-snap [_ kb]
    @conn)
  (-transact! [_ kb kbs dbs txs]
    (d/transact! conn txs)))

(defn new-datascript 
  ([] (new-datascript {}))
  ([opts] (map->Datascript opts)))
