(ns user
  (:require [onyx.sim.application :as app]
            [clojure.tools.namespace.repl :refer [set-refresh-dirs]]
            [reloaded.repl]))

(set-refresh-dirs "src" "dev")
(reloaded.repl/set-init! #(app/start (app/system)))

;; Set up aliases so they don't accidentally
;; get scrubbed from the namespace declaration
(def start reloaded.repl/start)
(def stop reloaded.repl/stop)
(def go reloaded.repl/go)
(def reset reloaded.repl/reset)
(def reset-all reloaded.repl/reset-all)
