(ns apps.bootstrap.cljs
  (:require cljs.build.api
            cljs.repl
            cljs.repl.browser
            cljs.repl.node
            [taoensso.timbre :as timbre]
            [clojure.java.io :as io]
            [cljs.closure :as closure]
            )
  (:import 
           java.net.URL))


(defn download-foreign-source
  "a bug in cljs compiler, online files cannot download automatically, so manually download here."
  ([to-dir]
   (-> "deps.cljs"
       io/resource
       slurp
       read-string
       (assoc :output-dir to-dir)
       timbre/spy
       ((fn [opts]
          (map (comp (partial closure/source-on-disk opts)
                     #(merge {:foreign true
                              :url (URL. (:file %))} %))
               (:foreign-libs opts))))
       doall))
  ([]
   (download-foreign-source ".")))


