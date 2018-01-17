(ns apps.bootstrap.cljs-rt-browser
  (:require [taoensso.timbre :as timbre]
            [clojure.java.io :as io]
            [hiccup.core :refer [html]]
            [hiccup.page :refer [include-js]]
            [hiccup.element :refer [javascript-tag]]
            [cljs.closure :as closure]
            cljs.build.api
            cljs.repl.browser
            cemerick.piggieback)
  (:import java.net.NetworkInterface
           java.net.URL))

(def repl-client-url (str "http://"
                          #_(-> (NetworkInterface/getNetworkInterfaces)
                              enumeration-seq
                              first
                              .getInetAddresses
                              enumeration-seq
                              last
                              .getHostAddress)
                          "192.168.124.7"
                          ":9000/repl"))

(def index-hcp
  [:html
   [:head]
   [:body
    (include-js "goog/base.js" "main.js")
    (javascript-tag (cljs.build.api/compile {} '(require 'clojure.browser.repl)))
    (javascript-tag (cljs.build.api/compile {} (apply list `(clojure.browser.repl/connect ~repl-client-url))))]])

(def index-path "out/index.html")

(def build-repl-opts
  {:output-to "out/main.js"
   ;; temporary files used during compilation. default to "out". base on assumption: all code will combine to ':output-to'.
   :output-dir "out"
   :browser-repl true
   :verbose true
   :repl-verbose true})

(defn download-foreign-source []
  (-> "deps.cljs"
      io/resource
      slurp
      read-string
      ((fn [opts]
         (map (comp (partial closure/source-on-disk opts)
                    #(merge {:foreign true
                             :url (URL. (:file %))} %))
              (:foreign-libs opts))))
      doall))

(defn -main []
  ;; a bug in cljs compiler, the remote js not download automatically, do download it before compile
  (download-foreign-source)

  ;; generate:
  ;; module system: goog/base.js, module info of google: goog/deps.js, module of google: goog/**
  ;; module info of other: main.js, module of others
  (cljs.build.api/build "src" build-repl-opts)

  ;; repl will serve static, so just generate index.html
  (spit index-path (html index-hcp))

  ;; exclude "." from static-dir to avoid overwrite of repl compiled files: out/out/** 
  (apply cemerick.piggieback/cljs-repl (cljs.repl.browser/repl-env :static-dir ["out/"]) (-> build-repl-opts
                                                                                             vec
                                                                                             flatten)))

#_(-main)

