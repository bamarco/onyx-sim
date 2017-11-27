(ns onyx.sim.svg
  (:require [taoensso.timbre :as log]
            [clojure.spec.alpha :as s]
            [onyx.sim.flui :as flui]
;;             #?(:cljs [vega-tools.core :refer [validate-and-parse]])
;;             #?(:cljs [cljsjs.vega])
            #?(:clj [onyx.sim.utils :refer [educe xfn ppr-str]]
               :cljs [onyx.sim.utils :refer [educe ppr-str] :refer-macros [xfn]])
            #?(:clj [clojure.math.numeric-tower :refer [sqrt]])
            #?(:clj  [clojure.core.match :refer [match]]
               :cljs [cljs.core.match :refer-macros [match]])))

;;;
;;; !!!: This is an experimental volatile file. Do not expect it to stay the same.
;;;


#?(:cljs (def sqrt (.-sqrt js/Math)))

(defonce scale 2.0)
(defonce shape-d (* scale 20))

(defn point->js-str [point]
  (apply str (interpose "," point)))

(defn points->js-str [points]
  ;; !!!: Security. Just building js strings is this okay???
  (apply str (interpose " " (map point->js-str points))))

(defn ->svg [{:as svg :keys [points]}]
  (let [svg (into svg {:points (points->js-str points)})
        tag (:svg/type svg)
        body (:svg/body svg)
        opts svg;;(select-keys svg (:svg/keys svg))
        ]
    (if tag
      [tag opts body]
      [:text {:x 0 :y 0 :font-size 14} (str "no ->svg tag for ")])))

(defn +bounding-box []
  (xfn [{:as svg :keys [width height padding]}]
       (into
         svg
         {:bound-width (+ width padding)
          :bound-height (+ height padding)
          :x (+ (/ padding  2))
          :y (+ (/ padding 2))})))

(defn +r []
  (xfn [{:as svg :keys [width]}]
    (assoc svg :r (/ width 2))))

(defn +center []
  (xfn [{:as svg :keys [x y cx cy width height]}]
    (into
      svg
      {:cx (or cx (+ x (/ width 2)))
       :cy (or cy (+ y (/ height 2)))})))

(defn +xy []
  (xfn [{:as svg :keys [x y cx cy width height]}]
    (into
      svg
      {:x (or x (- cx width))
       :y (or y (- cy height))})))

(defn +fill [color]
  (xfn [svg]
    (assoc svg :fill color)))

(defn +normalize []
  (xfn [{:as svg :keys [width]}]
         (assoc svg :height width)))

(defn +shape [tag]
  (xfn [svg] (assoc svg :svg/type tag)))

(defn +circumscribe-equi-tri []
  (comp
    (+center)
    (+r)
    (xfn [{:as svg :keys [cx cy r]}]
         (let [d (* 2 r)
               a [cx (- cy (/ (* (sqrt 3) r) 3))]
               b [(- cx r) (+ cy (/ (* (sqrt 3) d) 6))]
               c [(+ cx r) (+ cy (/ (* (sqrt 3) d) 6))]]
           (assoc svg :points [a b c])))))

(defn +zebra [& colors]
  (fn [step]
    (let [mod-current (volatile! 0)]
    (fn
      ([] (step))
      ([acc] (step acc))
      ([acc element]
        (let [modi @mod-current]
        (if [modi < (count colors)]
          (vreset! mod-current (inc modi))
          (vreset! mod-current 0))
          ;; ???: How should we handle differences between underlying container?
          (step acc (assoc-in element
                              [:fill] ;; svg
                              ;; [:style :background-color] ;; div
                              (get colors modi)))))))))

(defn x>rect []
  (comp
    (+xy)
    (+bounding-box)
    (+shape :rect)))


(defn x>square []
  (comp
    (+normalize)
    (x>rect)))

(defn x>circle []
  (comp
    (+normalize)
    (+bounding-box)
    (+center)
    (+r)
    (+shape :circle)))

(defn x>circumscribed-equi-tri []
  (comp
    (+normalize)
    (+bounding-box)
    (+circumscribe-equi-tri)
    (+shape :polygon)))

(defn x>number-text [n]
  (comp
    (+normalize)
    (+bounding-box)
    (+center)
    (+r)
    (xfn [{:as svg :keys [cx cy r]}]
         (into
           svg
           {:svg/type :text
            :svg/body n
            :x cx ;; ???: calc (- cx text-width) need to do this without using a js str
            :y cy
            :font-size r
            }
           ))))

(def dark-outline
  (xfn [svg]
    (into
      svg
      {:stroke "black"
       :stroke-width 3})))
(def no-outline
  (xfn [svg]
       (assoc svg :stroke :none)))

(def v-shape {:x 0 :y 0 :width shape-d :height shape-d :padding 5})

(def v-circle
  (educe
    (comp
      (x>circle)
      (+fill "Magenta")
      dark-outline)
    v-shape))

(def v-square
  (educe
    (comp
      (x>square)
      (+fill "LightGreen")
      dark-outline)
    v-shape))

(def v-rect
  (educe
    (comp
      (xfn [{:as svg :keys [height]}]
           (assoc svg :height (/ (* 2 height) 3)))
      (x>rect)
      (+fill "Cyan")
      dark-outline)
    v-shape))

(def v-triangle
  (educe
    (comp
      (x>circumscribed-equi-tri)
      (+fill "Cornsilk")
      dark-outline)
    v-shape))

(def v-quad
  [v-square
  (educe
    (comp
      (x>number-text 4)
      (+fill "White")
      no-outline)
    v-shape)])

(def padding 5)

(defn svg-orphan [{:as svg :keys [bound-width bound-height]} & more]
  (flui/box
    :child
    (into [:svg {:width bound-width :height bound-height} (->svg svg)]
          (map ->svg more)
          )))

(defn sides->shape [sides]
  (case sides
    1 :loop
    2 :error
    3 :tri
    4 :quad
    :poly))

(defn ^:export seg->svg-spec [seg]
  (log/debug "test-spec-here")
  (match
    [seg]
    [{:type :shape :kind :square}] v-square
    [{:type :shape :kind :circle}] v-circle
    [{:type :shape :kind :rect}] v-rect
    [{:type :shape :kind :triangle}] v-triangle
    [{:type :shape :kind :quad}] v-quad
    :else {:render/type :unmatched
           :seg seg}))

(defn svg-seg-boxer
  "Converts segments into individual svg elements."
  [& segs]
  [flui/h-box :children
   (into
     []
     (comp
       (map seg->svg-spec)
       (map ->svg)
       (map (fn [{:as svg :keys [bound-width bound-height]}]
              [flui/box
               :child
               [:svg {:width bound-width :height bound-height}]]))))
   segs])

(defn svg-seg-canvaser
  "Converts segments svg elements and puts them in one big svg container."
  [& segs]
  (let [total-bound-width (transduce (map :bound-width) + segs)
        total-bound-height (transduce (map :bound-height) max 0 segs)]
    [flui/box :child
     (into
       [:svg {:width total-bound-width
              :height total-bound-height}]
       (comp
         (map seg->svg-spec)
         (map ->svg)))
     segs]))

;; (defn +svg-boundary [& {:keys [align] :or {align :horizontal}}]
;;   (fn [step]
;;     (let [[wf hf] (case align
;;                     :horizontal [+ max]
;;                     :vertical [max +])
;;           width (volatile! 0)
;;           height (volatile! 0)
;;           ]
;;     (fn
;;       ([] (step))
;;       ([acc] {:svg/type :container
;;               :html/tag :svg
;;               :width width
;;               :height height})
;;       ([acc {:as seg :keys [bound-width bound-height]}]
;;        (let [bw (wf @width bound-width)
;;              bh (hf @height bound-height)]
;;          (vreset! width bw)
;;          (vreset! height bh)
;;          (step acc seg)))))))

(defn +svg-grow-bound [& {:keys [align] :or {align :horizontal}}]
  (fn [step]
    (let [[wf hf] (case align
                    :horizontal [+ max]
                    :vertical [max +])
          width (volatile! 0)
          height (volatile! 0)
          ]
    (fn
      ([] (step))
      ([acc] (step acc))
      ([acc {:as seg :keys [bound-width bound-height]}]
       (let [bw (wf @width bound-width)
             bh (hf @height bound-height)
             changed? (or (= @width bw) (= bh @height))
             acc (if changed? (step acc {:svg/type :container
                                         :html/tag :svg
                                         :width bw
                                         :height bh}) acc)]
         (vreset! width bw)
         (vreset! height bh)
         (step acc seg)))))))

(defn +svg-spread [& {:keys [align] :or {align :horizontal}}]
  (fn [step]
    (let [attr (case align
                 :horizontal :x
                 :vertical :y)
          width (volatile! 0)
          height (volatile! 0)]
      (fn
        ([] (step))
        ([acc] (step acc))
        ([acc svg]
         (if (= (:svg/type svg) :container)
           (do
             (vreset! width (:width svg))
             (vreset! height (:height svg))
             (step acc svg))
           (step acc (update-in svg [attr] + (case align
                                               :horizontal @width
                                               :vertical @height)))))))))

(defn svg-container->hiccup [{:as svg :keys [:html/tag :html/content]}]
  (if tag
    (into [tag (dissoc svg :html/tag :html/content :svg/type)] content)
    [:p (str "No :html/tag set for" svg)]))

(defn +svgrgggl [& {:as opts}]
  (fn [step]
    (let [container (volatile! {:svg/type :container
                                :html/tag :svg
                                :width 100
                                :height 100
                                :html/content []})]
      (fn
        ([] (step))
        ([acc] (let [c @container
                     acc (if c
                           (do
                             (vreset! container false)
                             (unreduced (step acc (svg-container->hiccup c))))
                           acc)]
                 (step acc)))
        ([acc svg]
         (if (= (:svg/type svg) :container)
           (vswap! container into svg)
           (vswap! container update-in [:html/content] conj (->svg svg)))
         acc)))))

(defn +boxed []
  (map (fn [svg-hiccup]
         [flui/box
          :child
          (or svg-hiccup flui/none)])))

;; (def seg->svg2
;;   (comp
;;     (map seg->svg-spec)
;;     (map ->svg)
;;     (+svg-boundary :align :horizontal)
;;     (+svgrgggl)
;;     (+boxed)))

(defn seg<x>svg []
  (comp
    (+svg-grow-bound :align :horizontal)
    (+svg-spread)
    (+svgrgggl)
    (+boxed)
;;     (map (fn [seg]
;;            [flui/p (str seg)]))
    ))

;; (defn svg-seg-reducer
;;   ([] {:html/tag flui/box
;;        :child {:db/id 1
;;                :html/tag :svg
;;                :width 0
;;                :height 0}}
;;    [flui/box :child [:svg {:width 0 :height 0}]])
;;   ([box svg] (transact! [[:db/add 1 :html/content ]])

;;   ))

;; tasks
(defn ^:export render-match
    ([] [])
    ([dom] (flui/h-box :children dom))
    ([dom seg]
     ;; FIXME: I definitely am doing this wrong. I need to figure out which parts need to be a transducer and which need to be the reducing function. You can tell by the travesty of a call that is in sim.cljc: (render (reduce render (render) outputs))
     ;; ???: how do we match containers to representations that go together. It seems like the container is the accumulation and the representations are the stream of inputs.
     ;; ???: how do we have a tree of transducers. Wait that sounds like the compute-graph. Somehow we need to be able to control the path the render should follow, maybe?
     ;; ???: how do we integrate transducers with onyx.
;;      (log/debug "rendering segment with keys" dom seg)
     (conj dom
           (match
             [seg]
             [{:transactions transactions}] (flui/box :child [:p (str "TXS: " (pr-str transactions))])
             [{:type :datom :eav [e a v]}] (flui/box :child [:p (str "EAV [" e " " a " " v "]")])
             [{:type :instance-sides :v v}] (flui/box :child [:p (str "SIDES: " v)])
             [{:type :instance-shape :v :square}] (svg-orphan v-square)
             [{:type :instance-shape :v :circle}] (svg-orphan v-circle)
             [{:type :instance-shape :v :rect}] (svg-orphan v-rect)
             [{:type :instance-shape :v :triangle}] (svg-orphan v-triangle)
             [{:type :instance-shape :v v}] (flui/box :child [:p (str "SHAPE: " v)]) ;; TODO: make a generic shape with a label equal to the keyword v
             [{:type :instance-4sides}] (apply svg-orphan v-quad)
             :else (flui/box :child [:p "fail"])))))
