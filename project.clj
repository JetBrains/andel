(defproject andel "0.1.0-SNAPSHOT"
  :description "Code editor component for web apps"
  :url "https://github.com/JetBrains/andel"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.9.0-alpha19"]                 
                 [cljsjs/react-dom "15.6.1-1"]
                 [cljsjs/react-dom-server "15.6.1-1"]
                 [cljsjs/create-react-class "15.6.0-1"]
                 [reagent "0.7.0"]
                 [reagent-utils "0.2.1"]                                  
                 [compojure "1.5.2"]
                 #_[hiccup "1.0.5"]               
                 [org.clojure/clojurescript "1.9.908"
                  :scope "provided"]
                 [cljs-http "0.1.43"]
                 [garden "1.3.2"]
                 [org.clojure/core.async "0.3.443"]]

  :plugins [[lein-environ "1.0.2"]
            [lein-cljsbuild "1.1.5"]
            [lein-asset-minifier "0.2.7"
             :exclusions [org.clojure/clojure]]]

  :min-lein-version "2.5.0"

  :uberjar-name "andel.jar"
  :clean-targets ^{:protect false}
  [:target-path
   [:cljsbuild :builds :app :compiler :output-dir]
   [:cljsbuild :builds :app :compiler :output-to]]

  :source-paths ["src/clj" "src/cljc"]
  :resource-paths ["resources" "target/cljsbuild"]

  :minify-assets
  {:assets {}}

  :cljsbuild
  {:builds {:min
            {:source-paths ["src/cljs" "src/cljc" "app/cljs"]
             :compiler
             {:output-to "target/cljsbuild/public/js/app.js"
              :output-dir "target/cljsbuild/public/js/"
              :main "andel.app"
              :optimizations :advanced
              :pseudo-names true
              :source-map "target/cljsbuild/public/js/app.js.map"
              :pretty-print  false
              :language-in :es5}}
            :app
            {:source-paths ["src/cljs" "src/cljc" "app/cljs"]
             :figwheel {:on-jsload "andel.app/init!"}
             :compiler
             {:main "andel.app"
              :asset-path "/js/out"
              :output-to "target/cljsbuild/public/js/app.js"
              :output-dir "target/cljsbuild/public/js/out"
              :source-map true
              :optimizations :none
              :pretty-print  true}}}}


  :figwheel
  {:http-server-root "public"
   :server-port 3449
   :nrepl-port 7002
   :nrepl-middleware ["cemerick.piggieback/wrap-cljs-repl"]
   :css-dirs ["resources/public/css"]
   }



  :profiles {:dev {:repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}
                   :dependencies [[ring/ring-mock "0.3.0"]
                                  [ring/ring-devel "1.5.1"]
                                  [prone "1.1.4"]
                                  [org.clojure/test.check "0.9.0"]
                                  [figwheel-sidecar "0.5.10"]
                                  [org.clojure/tools.nrepl "0.2.13"]
                                  [com.cemerick/piggieback "0.2.2-SNAPSHOT"]
                                  [pjstadig/humane-test-output "0.8.1"]]

                   :plugins [[lein-figwheel "0.5.10"]]

                   :injections [(require 'pjstadig.humane-test-output)
                                (pjstadig.humane-test-output/activate!)]

                   :env {:dev true}}

             :uberjar {:hooks [minify-assets.plugin/hooks]
                       :source-paths ["env/prod/clj"]
                       :prep-tasks ["compile" ["cljsbuild" "once" "min"]]
                       :env {:production true}
                       :aot :all
                       :omit-source true}})
