(ns apps.cljs-rt-browser
  (:require [clojure.tools.logging :as log]
            [clojure.java.io :as io]
            [hiccup.core :refer [html]]
            [hiccup.page :refer [include-js]]
            [hiccup.element :refer [javascript-tag]]
            cljs.build.api
            cljs.repl.browser
            cemerick.piggieback
            ))



(def repl-client-url "http://192.168.124.5:9000/repl")

(def index-hcp
  [:html
   [:head]
   [:body
    (include-js "goog/base.js" "main.js")
    (javascript-tag (cljs.build.api/compile {} '(require 'clojure.browser.repl)))
    (javascript-tag (cljs.build.api/compile {} (apply list `(clojure.browser.repl/connect ~repl-client-url))))]])

(def index-path "out/index.html")

(defn -main []
  ;; generate:
  ;; module system: goog/base.js, module info of google: goog/deps.js, module of google: goog/**
  ;; module info of other: main.js, module of others
  (cljs.build.api/build "src" {:output-to "out/main.js"
                               :browser-repl true
                               :verbose true
                               })

  ;; repl will serve static, so just generate index.html
  (when-not (. (io/file index-path) exists)
    (spit index-path (html index-hcp)))

  ;; exclude "." from static-dir to avoid overwrite of repl compiled files: out/out/** 
  (cemerick.piggieback/cljs-repl (cljs.repl.browser/repl-env :static-dir ["out/"]) :repl-verbose true))


#_(-main)

