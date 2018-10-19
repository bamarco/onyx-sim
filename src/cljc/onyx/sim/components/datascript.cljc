(ns onyx.sim.components.datascript
  (:require
    [taoensso.timbre :as log]
    [com.stuartsierra.component :as component]
    [onyx.sim.kb :as kb]
    [onyx.sim.substitutions :refer [substitute extract-db extract-inputs]]
    ;#?(:cljs [posh.reagent :as posh])
    [posh.plugin-base :as posh]
    #?(:cljs [reagent.ratom :refer [make-reaction]])
    [onyx.sim.api :as api]
    #?(:cljs [onyx.sim.console.ui :as ui])
    [datascript.core :as d]))

(defn- q-expr->ds-q-expr [{:as q-expr :onyx.sim.kb.datascript/keys [find where] :onyx.sim.kb/keys [in]}]
  {:find find
   :in (into ['$] (keys (dissoc in '$)))
   :where where})

(defn- q-expr->ds-pull [{:as q-expr :onyx.sim.kb.datascript/keys [pull-expr eid]} inputs]
  [(substitute pull-expr inputs) (substitute eid inputs)])

(defmethod kb/q :onyx.sim.kb.datascript/pull
  [kbs q-expr & {:as lk}]
  (let [db (extract-db kbs q-expr)
        inputs (extract-inputs kbs q-expr (first lk))
        [pull-expr eid] (q-expr->ds-pull q-expr inputs)]
    (d/pull db pull-expr eid)))

(defmethod kb/sub :onyx.sim.kb.datascript/pull
  [kb q-expr & {:as lk}]
  ;(log/info "datascript/pull")
  (let [conn (:conn (extract-db kb q-expr))
        ; _ (log/info "kpconn" conn)
        inputs (extract-inputs kb q-expr lk)
        ;_ (log/info "inputs" inputs)
        [pull-expr eid] (q-expr->ds-pull q-expr inputs)
        ;_ (log/info "k e e" pull-expr eid)
        reaction #?(:cljs
                    (posh/pull conn pull-expr eid)
                    :clj
                    (let [p (promise)]
                      (deliver (d/pull conn pull-expr eid))
                      p))]
    ;(log/info "reaction" reaction)
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
  (let [conn (:conn (extract-db kb q-expr))
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
      ;#?(:cljs (posh/posh! conn))
      (assoc component
        :conn conn)))
  (stop [component]
    (assoc component
      :conn nil))
  kb/DB
  (-snap [_ kb]
    @conn))
  ; (-transact! [_ kb kbs dbs txs]
  ;   (d/transact! conn txs)))

(defn new-datascript 
  ([] (new-datascript {}))
  ([opts] (map->Datascript opts)))
