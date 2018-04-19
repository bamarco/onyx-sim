(ns onyx.sim.dat-view-api
  (:require
    [taoensso.timbre :as log]
    [onyx.sim.api :as onyx]
    [clojure.core.async :as async]))

(defmulti render-segments 
  (fn [system segments]
    (let [render-type (some :dat.view.render/type segments)]
      (log/info "render-segments dispatch:" render-type)
      render-type)))
    

(defn dispatch! [event-jobs & args]
  (doseq [event-job event-jobs]
    (onyx/submit-job 
      (update-in 
        event-job 
        [:catalog] 
        conj 
        {:onyx/name           ::event-args
         :onyx/type           :input
         :onyx/plugin         :onyx.plugin.seq
         :onyx.plugin.seq/seq args}))))

(def quick-render (partial render-segments {:dat.view/dispatch! dispatch!}))

(defn render-job [job]
  (let [env  (onyx/submit-job->ratom job)
        segs (get-in @env [:tasks :dat.view.render/mount :outputs])]
    [quick-render segs]))

(defn output-plugin [{:as plugin :keys [dat.view/mount-point]}]
  (assoc plugin :dat.view.render/render-chan (async/chan)))

(def mount-task
  (output-plugin
    {:onyx/name                   :dat.view.render/mount
     :onyx/type                   :output
     :dat.view.render/mount-point "app"
     :onyx/plugin                 :dat.view.plugin/render}))
