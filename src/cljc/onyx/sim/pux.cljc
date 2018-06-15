(ns onyx.sim.pux
  "Persona User Experience"
  (:require
    [clojure.spec.alpha :as s]
    [clojure.set :as set]))

;; TODO: move pux to its own project
;; TODO: integrate with buddy

(s/def :pux.core/role keyword?)

(s/def :pux.core/roles (s/coll-of :pux.core/role))

(s/def :pux.core/persona (s/keys :req [:pux.core/roles]))

(s/def :pux.core/personas (s/coll-of :pux.core/persona))

(s/def :pux.core/user (s/keys :req [:pux.core/personas]))

(s/def :pux.core/required :pux.core/roles)

;(s/def :pux.core/preffered (graph-of :pux.core/role))

(s/def :pux.core/forbidden :pux.core/roles)

(s/def :pux.core/sentinel (s/keys :opt [:pux.core/required :pux.core/forbidden]))

(defn guard 
  "True if sentinel allows persona through"
  [{:as sentinel :pux.core/keys [required forbidden]} {:as persona :pux.core/keys [roles]}]
  (and
    (s/valid? :pux.core/sentinel sentinel)
    (s/valid? :pux.core/persona persona)
    (set/subset? (set required) (set roles))
    (empty? (set/intersection (set forbidden) (set roles)))))

; (defn match 
;   "Pass a persona through several sentinels"
;   [persona & matches]
;   ;; ???: needs to be a macro to protect from running statements with side effects?
;   ;; FIXME: infinite loop on no matches
;   ;; TODO: :else
;   (loop [matches matches]
;     (let [sentinel (first matches)
;           statement (second matches)
;           more-matches (rest (rest matches))]
;       (if (guard sentinel persona)
;         statement
;         (recur more-matches)))))

; (defn orient 
;   "Adjusts the model based on the persona. Settings might be different for example"
;   [{:as persona :pux.core/keys []} model]
;   ;; ???: rules based?
;   ;; ???: aquire persona from the model while orienting
;   ;; ???: aquire persona from the model everywhere
;   model)
  
