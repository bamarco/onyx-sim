(defproject onyx-sim "0.1.1-SNAPSHOT"
  :description "A simulator for the onyx runtime."
  :url "https://github.com/bamarco/onyx-sim"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies 
  [
    [org.clojure/clojure "1.10.0-alpha4"]
    [org.clojure/clojurescript "1.10.238"]
    [org.clojure/core.async "0.4.474"]
    [datascript "0.16.4"]
    [re-com "2.1.0"]
    [com.cognitect/transit-clj "0.8.300"]
    [cljs-http "0.1.44"]
    [ring "1.6.3"]
    [ring/ring-defaults "0.3.1"]
    [bk/ring-gzip "0.3.0"]
    [ring-logger-timbre "0.7.6"]
    [compojure "1.6.0"]
    [environ "1.1.0"]
    [com.stuartsierra/component "0.3.2"]
    [org.danielsz/system "0.4.1"]
    [com.rpl/specter "1.1.1"]
    [reagent "0.7.0"]
    [posh "0.5.5"]
    [clj-time "0.14.4"]
    [com.andrewmcveigh/cljs-time "0.5.2"]
    [com.taoensso/timbre "4.10.0"]
    [prismatic/schema "1.1.7"]
    [org.onyxplatform/onyx-spec "0.13.0.1-SNAPSHOT"]
    [org.onyxplatform/onyx "0.13.1-SNAPSHOT"]
    [org.onyxplatform/onyx-local-rt "0.13.0.1-SNAPSHOT"]]

  :min-lein-version "2.6.1"

  :source-paths ["src/clj" "src/cljs" "src/cljc"]

  :test-paths ["test/clj" "test/cljc"]

  :clean-targets ^{:protect false} [:target-path :compile-path "resources/public/js"]

  :uberjar-name "onyx-sim.jar"

  :plugins [[lein-cljsbuild "1.1.6"]
            [lein-environ "1.1.0"]]

  ;; Use `lein run` if you just want to start a HTTP server, without figwheel
  :main onyx.sim.application

  ;; nREPL by default starts in the :main namespace, we want to start in `user`
  ;; because that's where our development helper functions like (go) and
  ;; (browser-repl) live.
  :repl-options {:init-ns user
                 :port 4334}

  :cljsbuild {:builds
              [{:id "app"
                :source-paths ["src/cljs" "src/cljc"
                               "dev"]

                :figwheel {:on-jsload "onyx.sim.system/reset"}

                :compiler {:main cljs.user
                           :asset-path "js/compiled/out"
                           :output-to "resources/public/js/compiled/onyx_sim.js"
                           :output-dir "resources/public/js/compiled/out"
                           :source-map-timestamp true}}

               {:id "test"
                :source-paths ["src/cljs" "test/cljs" "src/cljc" "test/cljc"]
                :compiler {:output-to "resources/public/js/compiled_test/onyx_sim_test.js"
                           :output-dir "resources/public/js/compiled_test/out"
                           :main onyx.sim.test-runner
                           :optimizations :none}}

               {:id "min"
                :source-paths ["src/cljs" "src/cljc"]
                :jar true
                :compiler {:main onyx.sim.system
                           :output-to "resources/public/js/compiled/onyx_sim.js"
                           :output-dir "target"
                           :source-map-timestamp true
                           :optimizations :advanced
                           :pretty-print false}}]}

  ;; When running figwheel from nREPL, figwheel will read this configuration
  ;; stanza, but it will read it without passing through leiningen's profile
  ;; merging. So don't put a :figwheel section under the :dev profile, it will
  ;; not be picked up, instead configure figwheel here on the top level.

  :figwheel {;; :http-server-root "public"       ;; serve static assets from resources/public/
             ;; :server-port 3449                ;; default
             ;; :server-ip "127.0.0.1"           ;; default
             :css-dirs ["resources/public/css" "resources/public/ns"]  ;; watch and update CSS

             ;; Start an nREPL server into the running figwheel process. We
             ;; don't do this, instead we do the opposite, running figwheel from
             ;; an nREPL process, see
             ;; https://github.com/bhauman/lein-figwheel/wiki/Using-the-Figwheel-REPL-within-NRepl
             ;; :nrepl-port 7888

             ;; To be able to open files in your editor from the heads up display
             ;; you will need to put a script on your path.
             ;; that script will have to take a file path and a line number
             ;; ie. in  ~/bin/myfile-opener
             ;; #! /bin/sh
             ;; emacsclient -n +$2 $1
             ;;
             ;; :open-file-command "myfile-opener"

             :server-logfile "log/figwheel.log"}

  :doo {:build "test"
        :alias {:default [:chrome-headless]}}

  :profiles {:dev
             {:dependencies [[figwheel "0.5.15"]
                             [figwheel-sidecar "0.5.15"]
                             [com.cemerick/piggieback "0.2.2"]
                             [org.clojure/tools.nrepl "0.2.13"]
                             [reloaded.repl "0.2.3"]]

              :plugins [[lein-figwheel "0.5.15"]
                        [lein-doo "0.1.8"]
                        [lein-pprint "1.1.1"]]
                        

              :source-paths ["dev"]
              :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}}

             :uberjar
             {:source-paths ^:replace ["src/clj" "src/cljc"]
              :prep-tasks ["compile"
                           ["cljsbuild" "once" "min"]]
              :hooks []
              :omit-source true
              :aot :all}})
