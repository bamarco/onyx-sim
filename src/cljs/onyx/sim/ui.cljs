(ns onyx.sim.ui
  (:require
    [re-com.core :as re-com]
    [onyx.sim.utils :as utils :refer [cat-into ppr-str mapply]]))

(def none [:span])

(defn code
  "Eventually a pretty lookin code block with syntax highlighting."
  [& {:as args :keys [code child pr-fn] cl :class}]
  ;; TODO: word-wrap and line numbers
  ;; TODO: syntax highlighting
  (let [args (-> args
                (dissoc :code :pr-fn)
                (assoc :class (str "rc-code " cl)))
        code ((or pr-fn ppr-str) code)]
    (assert (not child) (str "Code should not have a :child element. Got " child))
    (mapply re-com/box :child [:code [:pre code]] args)))
