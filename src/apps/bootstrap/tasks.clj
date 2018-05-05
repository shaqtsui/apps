(ns apps.bootstrap.tasks
  (:require [boot.core :refer [deftask set-env!]]
            [boot.task.built-in :refer :all]
            [adzerk.boot-cljs :refer [cljs]]
            [adzerk.boot-reload :refer [reload]]
            [metosin.boot-alt-http :refer [serve]]
            [adzerk.boot-cljs-repl :refer [cljs-repl start-repl]]
            [adzerk.boot-test :refer [test]]
            [crisptrutski.boot-cljs-test :refer [test-cljs]]
            [taoensso.timbre :as timbre]
            [clojure.pprint :refer [pprint]]
            [clojure.java.io :as io]
            [apps.bootstrap.cljs :refer [download-foreign-source index-html]]
            [apps.dcs :refer [as]]))

(deftask repo-crdtl
  "Not a wrapper, just add credentials
  Usage: boot pom jar repo-crdtl -i 0 -u yourname -p yourpassword push -r clojars"
  [i index VALUE int "repo index"
   u username VALUE str "username"
   p password VALUE str "password"]
  (set-env! :repositories #(assoc-in % [index 1 :username] username))
  (set-env! :repositories #(assoc-in % [index 1 :password] password))
  identity)

(deftask testing
  "Not a wrapper, just add test for CLJ/CLJS testing purpose"
  []
  (set-env! :source-paths #(conj % "test"))
  identity)

(deftask download-online-js
  "download online js in deps.cljs"
  []
  (fn wrapper [next-handler]
    #(-> %
         (doto
          (as $ (timbre/debug "start download-online-js..."))
           (as $ (download-foreign-source)))
         next-handler)))

(deftask add-index
  []
  (fn wrapper [next-handler]
    #(-> %
         (doto (as $ (timbre/debug "start add-index...")))
         (boot.core/add-resource (doto
                                  (boot.core/tmp-dir!)
                                   (-> (io/file "public/index.html")
                                       (doto io/make-parents)
                                       (spit index-html))))
         boot.core/commit!
         next-handler)))

(deftask dev
  "lanch IFDE"
  []
  (comp
   (add-index)
   (download-online-js)
   ;; directly serve files from temp folder
   #_(serve :directories #{"target/public"} :port 3000)
   (serve :port 3000)
   (watch)
   ;; put before cljs as they need to update .cljs.edn
   (reload)
   (cljs-repl)
   ;; I don't like to put cljs task options in .cljs.edn, as this is part of bootstrap config.
   ;; but currently only .cljs.edn can config :require. so I have no choice to only put :require in .clj.edn
   (cljs :compiler-options {:asset-path "js/main.out"
                            :output-dir "public/js/main.out"
                            :output-to "public/js/main.js"})
   ;; directly serve files from temp folder
   #_(target)))

(deftask tdd
  "Lanch tdd env"
  []
  (comp
   (testing)
   (watch)
   (test-cljs :update-fs? true :phantom :namespaces '#{apps.test})
   (test :namespaces '#{apps.test})))
