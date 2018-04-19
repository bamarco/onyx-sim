(ns onyx.sim.dat-view-render
  (:require 
    [taoensso.timbre :as log]
    [re-com.core :as re-com]
    [reagent.core :as reagent]
    [clojure.core.async :as async :refer [go-loop <! >!]]
    [onyx.sim.dat-view-api :as api]))

(def render-segments api/render-segments)
(def mount-task api/mount-task)
(def quick-render api/quick-render)

; (defmethod api/render-segments ::box 
;   [_ attrs]
;   (let [children  (filter ::child   attrs)
;         direction (some   ::direction attrs)]
;     [(if (= ::horizontal direction) re-com/h-box re-com/v-box)
;      :children
;      (mapv render-job children)]))

; (defmethod api/render-segments ::label 
;   [_ attrs]
;   (let [(some ::label attrs)]
;     [re-com/label :label label]))

; (defmethod api/render-segments ::text 
;   [{:keys [dat.view/dispatch!]} attrs]
;   (let [text      (some ::text      attrs)
;         on-change (some ::on-change attrs)]
;     [re-com/input-text
;      :model     text
;      :on-change (partial dispatch! on-change)]))

(defmethod api/render-segments :dat.view.render/nav 
  [{:keys [dat.view/dispatch!]} attrs]
  (log/info "render nav !!!!!!!")
  (let [choices (into
                  []
                  (comp 
                    (map :dat.view/choice)
                    (remove nil?))
                  attrs)
        _ (log/info "choices" choices)
        id-attr    (:dat.view/attr (some :dat.view/id-attr attrs))
        _ (log/info "id-attr" id-attr)
        chosen     (id-attr (some :dat.view/chosen attrs))
        _ (log/info "chosen" chosen)
        label-attr (:dat.view/attr (some :dat.view/label-attr attrs))
        _ (log/info "label-attr" label-attr)
        on-change  (filterv :dat.view/on-change attrs)
        _ (log/info "on-change" on-change)]
    [re-com/horizontal-bar-tabs
      :tabs      choices
      :model     chosen
      :id-fn     (or id-attr :dat.view/id)
      :label-fn  (or label-attr :dat.view/label)
      :on-change (partial dispatch! on-change)]))

(defn go-render-mount-point [{:as render-plugin :keys [dat.view.render/mount-point dat.view.render/render-chan]}]
  (go-loop []
    (let [segs (<! render-chan)]
      ;; FIXME: segs need to be collected
      (reagent/render [render-segments segs] mount-point))))
