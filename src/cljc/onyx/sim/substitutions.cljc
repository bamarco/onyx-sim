(ns onyx.sim.substitutions
  (:require
    [com.rpl.specter :as specter]))

(defn substitute [form inputs]
  ; (log/info "subst" form inputs)
  (specter/transform
    (specter/walker symbol?)
    (fn [sym]
      ; (log/info "checking " sym " for replace " (get inputs sym))
      (if-let [replacement (get inputs sym)]
        replacement
        sym))
    form))

(defn extract-inputs [kb* {:as q-expr :onyx.sim.kb/keys [in]} local-knowledge]
  ; (log/info "xtract")
  ; (log/info "xtr-in" in)
  ; (log/info "xlocal" local-knowledge)
  (let [knowledge (merge kb* local-knowledge)]
    (into {}
      (for [[sym k] in]
        [sym (get knowledge k)]))))

(defn extract-db [kb* {:as q-expr :onyx.sim.kb/keys [in]}]
  (get kb* (get in '$)))
