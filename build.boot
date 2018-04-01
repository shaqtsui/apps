(set-env!
 :source-paths #{"src"}
 :resource-paths #{"resources"}
 :dependencies '[;; here not used as runtime, runtime specified in boot.properties, just have this here to cut other indirect deps of clojure
                 [org.clojure/clojure "1.9.0"]
                 [org.clojure/core.async "0.4.474" :exclusions [org.ow2.asm/asm-all]]
                 [org.clojure/tools.logging "0.4.0"]
                 [org.clojure/tools.reader "1.1.1"]
                 [org.clojure/tools.nrepl "0.2.13"]
                 [org.clojure/clojurescript "1.9.946"]
                 [org.clojure/math.numeric-tower "0.0.4"]

                 ;; logging
                 [com.taoensso/timbre "4.10.0"]
                 [com.fzakaria/slf4j-timbre "0.3.8"]
                 [org.slf4j/jcl-over-slf4j "1.7.25"]
                 ;; apps.nrepl-cider
                 [cider/cider-nrepl "0.16.0"]
                 ;; seems weasel is optional, I can integrate cider with piggieback to support browser support. It's bad to introduce weasel api into my cljs file                 
                 [com.cemerick/piggieback "0.2.2"]
                 ;; boot task boot-cljs-repl generate cljs repl conection code for me base on weasel
                 [weasel "0.7.0"]
                 ;; for flycheck-clojure 
                 [acyclic/squiggly-clojure "0.1.9-SNAPSHOT"]

                 ;; cms
                 [ring "1.6.3"]
                 [ring/ring-defaults "0.3.1"]
                 [ring-webjars "0.2.0" :exclusions [com.fasterxml.jackson.core/jackson-core]]
                 [compojure "1.6.0"]
                 [hiccup "1.0.5"]
                 [hickory "0.7.1"]
                 [clj-org "0.0.2"]

                 ;; my licence expire on 2017/05/17, upgrade not supported, so only 0.9.5561 supported. can register new account?
                 ;; this is free but not for open source
                 ;;[com.datomic/datomic-pro "0.9.5561" :exclusions [commons-codec org.apache.httpcomponents/httpclient org.slf4j/slf4j-nop]]
                 [com.datomic/datomic-free "0.9.5656" :exclusions [commons-codec org.apache.httpcomponents/httpclient org.slf4j/slf4j-nop com.google.guava/guava]]

                 [org.webjars.bower/tether "1.4.3"]
                 [org.webjars/bootstrap "4.0.0-beta.3"]
                 [org.webjars.bower/semantic "2.2.13"]
                 [endophile "0.2.1"]
                 [buddy/buddy-auth "2.1.0"]
                 [oauth-clj "0.1.15" :exclusions [commons-logging]]
                 [org.danielsz/system "0.4.1"]
                 [com.cemerick/pomegranate "1.0.0" :exclusions [commons-logging]]
                 [prone "1.1.4"]
                 [ring-logger "0.7.7"]

                 ;; for tools
                 [enlive "1.1.6"]
                 [com.draines/postal "2.0.2"]
                 ;; for incanter-example
                 [net.mikera/core.matrix "0.62.0"]
                 ;; core.matrix implementations
                 [clatrix "0.5.0"]
                 [net.mikera/vectorz-clj "0.47.0"]
                 ;; can not get the latest impl now
                 [org.clojars.xfcjscn/nd4clj "0.1.2"]
                 
                 ;; buddy contains a copy for jdk15
                 [incanter "1.9.2" :exclusions [org.bouncycastle/bctsp-jdk14 bouncycastle/bcmail-jdk14 bouncycastle/bcprov-jdk14]]
                 ;; for ml week 4
                 [net.mikera/imagez "0.12.0"]

                 ;; func-plot
                 [prismatic/dommy "1.1.0"]
                 [hipo "0.5.2"]
                 [cljsjs/three "0.0.87-0"]
                 ;; browser control, PREREQUISITE: chromedriver to be in $PATH
                 [etaoin "0.2.4"]

                 ;; for boot(bootstrap)
                 [adzerk/boot-cljs "2.1.4"]
                 [adzerk/boot-reload "0.5.2"]
                 ;; can not use [pandeiro/boot-http "0.8.3"], as internally ring 1.4.0 dependency hardcoded in source
                 ;; here boot-alt-http also hard code dependency, but it's ring 1.6.3
                 [metosin/boot-alt-http "0.2.0"]
                 [adzerk/boot-cljs-repl   "0.3.3"]
                 [adzerk/boot-test "1.2.0"]
                 [crisptrutski/boot-cljs-test "0.3.4"]])

(task-options!
 pom {:project 'apps
      :version "0.1.0-SNAPSHOT"}
 ;; cider don't inject this automaticlly, so duplicate with code in nrepl-cider
 repl {:middleware '[cemerick.piggieback/wrap-cljs-repl]})

(require '[adzerk.boot-cljs :refer [cljs]]
         '[adzerk.boot-reload :refer [reload]]
         '[metosin.boot-alt-http :refer [serve]]
         '[adzerk.boot-cljs-repl :refer [cljs-repl start-repl]]
         '[apps.bootstrap.cljs :refer [download-foreign-source index-html]]
         '[apps.dcs :refer [as]]
         '[adzerk.boot-test :refer [test]]
         '[crisptrutski.boot-cljs-test :refer [test-cljs]]
         '[taoensso.timbre :as timbre]
         '[clojure.pprint :refer [pprint]]
         '[clojure.java.io :as io])

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

(deftask testing
  "Not a wrapper, just add test for CLJ/CLJS testing purpose"
  []
  (set-env! :source-paths #(conj % "test"))
  identity)

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
