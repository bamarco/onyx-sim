(ns onyx.sim.utils
  (:require 
    [taoensso.timbre :as log]
    [clojure.pprint :refer [pprint]]
    [clojure.core.async.impl.protocols :refer [WritePort ReadPort Channel]]
    #?(:clj [clojure.edn :as reader])
    [datascript.core :as d]
    [clojure.core.async :refer [<! go go-loop]]
    #?(:clj [clojure.java.io :as io]))
  #?(:cljs 
     (:require-macros 
        [onyx.sim.utils :refer [xfn stepper <apply go-let controlled-loop]])))
        ; [cljs.core.async.macros :refer [go]])))

;;;
;;; !!!: This is an experimental volatile file. Do not expect it to stay the same.
;;;

(defn gen-uuid []
  (d/squuid))

#?(:clj
   (defn edn-seq
     "Returns the objects from stream as a lazy sequence."
     ([]
      (edn-seq *in*))
     ([stream]
      (edn-seq {} stream))
     ([opts stream]
      (lazy-seq (cons (reader/read opts stream) (edn-seq opts stream))))))

#?(:clj
   (defn swallow-eof
     "Ignore an EOF exception raised when consuming seq."
     [seq]
     (-> (try
           (cons (first seq) (swallow-eof (rest seq)))
           (catch java.lang.RuntimeException e
             (when-not (= (.getMessage e) "EOF while reading")
               (throw e))))
         lazy-seq)))

#?(:clj
   (defn edn-read-file [filename]
     (if (io/.exists (io/as-file filename))
       (with-open [stream (java.io.PushbackReader. (io/reader filename))]
         (doall (swallow-eof (edn-seq stream))))
       (println (str "File does not exist: " filename)))))

(defonce last-tempid (atom 0))
(defn gen-tempid!
  "Generate a tempid for datascript. You can use this generator XOR your own, but NOT BOTH because there could be clashes. Only call from inside a db-fn"
  [& {:keys [wrap-on-overflow? warn-on-overflow?]}]
  (let [id (swap! last-tempid dec)]
    (assert (> id -1000000) "Ran out of tempids. To generate more this gen-tempid function will need to be patched to partition tempids or otherwise.")
    id))

(defn deref-or-value
  "Derefs when able, otherwise the value."
  [val-or-atom]
  (if (satisfies? #?(:cljs IDeref :clj clojure.lang.IDeref) val-or-atom) @val-or-atom val-or-atom))

#?(:clj
   (defmacro xfn
     "Simple transducing function."
     [bindings & body]
     `(map (fn ~bindings ~@body))))

#?(:clj
   (defmacro stepper
     "Allows you to quickly create completing-style transducers."
     [[step acc in] & body]
     `(fn [~step]
       (fn
         ([] (~step))
         ([~acc] (~step ~acc))
         ([~acc ~in]
          ~@body)))))

(defn expand
  "When predicate is true, push elements into the stream instead of the element."
  [predicate coll]
  (stepper [step acc in]
    (if (predicate in)
      (reduce step acc coll)
      (step acc in))))

(defn educe
  "For applying a transducer to a single item."
  [xf item]
  (first (eduction xf [item])))

(defn ppr-str [& args]
  (apply 
    str 
    (map 
      #(if (string? %) 
        % 
        (with-out-str (pprint %))) 
      args)))

;;
;; cat-into takes a collection to merge into, any number of transducers, and at least one sequence. The sequences are treated as a single sequence back to back.
;;
;; args -
;;   coll [transucers ...]* [sequences ...]+
;;
(defn cat-into
  "Any number of transducers and sequences concatonated into one collection."
  [coll & xfns-and-seqs]
  (let [{xfns true
         seqs false} (if (fn? (first xfns-and-seqs))
                       (group-by fn? xfns-and-seqs)
                       {false xfns-and-seqs})]
                       
    (into coll
          (apply comp (into [cat] xfns))
          seqs)))

(defn fmap
  "Apply function to the values of a hash map."
  [f dict]
  ;; FIXME: not robust only handles single {}
  (into {} (map (fn [[k v]]
                  [k (f v)])) dict))

(defn mapply
  "Apply a function with a map keys and values added to the argument list. Useful for functions with map-destructing of variadic functions e.g. (defn foo [& {:as m :keys [bar]}] bar)"
  [f & args]
  (apply f (apply concat (butlast args) (last args))))

(defn minto
  "Like mapply, but for vectors. ???: Not robust needs upgrades."
  ([in from]
   (into in cat from))
  ([in xform from]
   (into in (comp cat xform) from)))

(defn deep-merge
  "Like merge, but merges maps recursively."
  [& maps]
  (if (every? #(or (map? %) (nil? %)) maps)
    (apply merge-with deep-merge maps)
    (last maps)))

(defn third 
  "Is third is used often enough to have it's own name? Should the need to use third be a warning to name things?"
  [coll]
  (nth coll 2))

#?
(:clj
  (defmacro forv
    "for loop as a vector. Equivalent to mapv"
    [bind & body]
    `(vec (for ~bind ~@body))))

#?
(:clj
  (defmacro controlled-loop 
    "Create a go-loop that returns a control chan. Intended to be used in conjunction with alts!"
    [control> & body]
    `(let [~(symbol (namespace control>) (name control>)) (or ~control> (async/chan))]
      (go-loop [] ~@body)
      ~control>)))

(defn read-chan? [obj] 
  (satisfies? ReadPort obj))

#?
(:clj
  (defmacro <apply
    "If the function returns a chan take a segment from it. Meant for use with a promise chan or for returning a go block. Rationale: The go macro stops translating at the fn barrier and sometimes a function only optionally needs to perform asynchronous operations"
    [f & args]
    `(let [seg-or-chan# (~f ~@args)]
       (if (read-chan? seg-or-chan#)
         (<! seg-or-chan#)
         seg-or-chan#))))

#?
(:clj
  (defmacro go-let
    "If the item is a chan take and execute body inside a go-block. Otherwise just execute the body. Use in conjunction with <apply when calling a function that returns a go-let block"
    [[sym seg-or-chan] & body]
    `(if (read-chan? ~seg-or-chan)
       (go
        (let [~sym (<! ~seg-or-chan)]
          ~@body))
       (let [~sym ~seg-or-chan]
         ~@body))))
      
        

