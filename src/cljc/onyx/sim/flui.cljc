(ns onyx.sim.flui
  (:require [taoensso.timbre :as log]
            #?(:cljs [re-com.core :as rc])
            [onyx.sim.utils :refer [mapply ppr-str]]))

(defn call
  "Because you can't use reagent on the serverside. :("
  [f & args]
  #?(:cljs (into [f] args)
     :clj (apply f args)))

(defn stub
  "Provides a stub for functions that aren't implemented yet."
  [fn-name]
  (fn [& args]
    [:div.stub [:p (str "STUB for: " (pr-str (cons fn-name args)))]]))

#?
(:cljs
  (defn code*
    "Eventually a pretty lookin code block with syntax highlighting."
    [& {:as args :keys [code child] cl :class}]
    ;; TODO: word-wrap and line numbers
    ;; TODO: syntax highlighting
    (let [args (-> args
                   (dissoc :code)
                   (assoc :class (str "rc-code " cl )))
          code (ppr-str code)]
      (assert (not child) (str "Code should not have a :child element. Got " child))
      (mapply rc/box :child [:code [:pre code]] args))))


(def none [:span])
(def code
  #?(:clj (stub 'code)
     :cljs code*))
(def button
  #?(:clj (stub 'button)
     :cljs (partial call rc/button)))
(def radio-button
  #?(:clj (stub 'radio-button)
     :cljs (partial call rc/radio-button)))
(def gap
  #?(:clj (stub 'gap)
     :cljs (partial call rc/gap)))
(def p
  #?(:clj (stub 'p)
     :cljs (partial call rc/p)))
(def horizontal-tabs
  #?(:clj (stub 'horizontal-tabs)
     :cljs (partial call rc/horizontal-tabs)))
(def horizontal-pill-tabs
  #?(:clj (stub 'horizontal-tabs)
     :cljs (partial call rc/horizontal-pill-tabs)))
(def horizontal-bar-tabs
  #?(:clj (stub 'horizontal-tabs)
     :cljs (partial call rc/horizontal-bar-tabs)))
(def checkbox
  #?(:clj (stub 'checkbox)
     :cljs (partial call rc/checkbox)))
(def h-box
  #?(:clj (stub 'h-box)
     :cljs (partial call rc/h-box)))
(def v-box
  #?(:clj (stub 'v-box)
     :cljs (partial call rc/v-box)))
(def box
  #?(:clj (stub 'box)
     :cljs (partial call rc/box)))
(def label
  #?(:clj (stub 'label)
     :cljs (partial call rc/label)))
(def title
  #?(:clj (stub 'title)
     :cljs (partial call rc/title)))
(def input-text
  #?(:clj (stub 'input-text)
     :cljs (partial call rc/input-text)))
(def input-textarea
  #?(:clj (stub 'input-textarea)
     :cljs (partial call rc/input-textarea)))
(def selection-list
  #?(:clj (stub 'selection-list)
     :cljs (partial call rc/selection-list)))
(def single-dropdown
  #?(:clj (stub 'single-dropdown)
     :cljs (partial call rc/single-dropdown)))
