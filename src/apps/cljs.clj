(ns apps.cljs
  (:require cljs.build.api
            cljs.repl
            cljs.repl.browser
            cljs.repl.node))

(cljs.build.api/build "src" {:output-to "out/main.js"
                             ;; 1, more dependency will be added to goog. 2, if main specified, load module connect to repl (a bug: connect to hardcoded localhost:9000 )
                             :browser-repl true
                             :verbose true
                             ;; if main not specifed, only cljs_deps.js's contents will be in output
                             ;; :main 'apps.hello-world
                             ;; :asset-path ""
                             })

#_(cljs.build.api/watch "src" {:output-to "out/main.js"
                             :browser-repl true
                               :verbose true})

;; use: java -cp $(lein classpath) clojure.main src/apps/cljs.clj to start the cljs repl
#_(cljs.repl/repl (cljs.repl.browser/repl-env)
                :watch "src"
                :output-dir "out")


;; for nodejs build
#_(cljs.build.api/build "src" {:main 'apps.hello-world-nodejs
                             :output-to "out/main.js"
                             :verbose true
                             :target :nodejs})

#_(cljs.repl/repl (cljs.repl.node/repl-env)
                :watch "src"
                :output-dir "out")
