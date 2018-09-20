(def dependencies
  (-> {:core '[[org.clojure/clojure "1.9.0"]
               [org.clojure/core.async "0.4.474" :exclusions [org.ow2.asm/asm-all]]
               [org.clojure/tools.logging "0.4.0"]
               [org.clojure/tools.reader "1.3.0"]
               [org.clojure/clojurescript "1.10.339"]
               [org.clojure/math.numeric-tower "0.0.4"]
               [org.clojure/data.csv "0.1.4"]
               [org.clojure/data.json "0.2.6"]
               ]
       :app '[[nrepl "0.4.5"]
              ;; logging
              [com.taoensso/timbre "4.10.0"]
              [com.taoensso/tufte "2.0.1"]
              [com.fzakaria/slf4j-timbre "0.3.12"]
              [org.slf4j/jcl-over-slf4j "1.7.25"]
              ;; apps.nrepl-cider
              [cider/cider-nrepl "0.18.0"]
              ;; seems weasel is optional, I can integrate cider with piggieback to support browser support. It's bad to introduce weasel api into my cljs file                 
              [com.cemerick/piggieback "0.2.2"]
              ;; boot task boot-cljs-repl generate cljs repl conection code for me base on weasel
              [weasel "0.7.0"]
              ;; for flycheck-clojure 
              [acyclic/squiggly-clojure "0.1.8"]

              ;; cms
              [ring "1.6.3"]
              [ring/ring-defaults "0.3.1"]
              [ring-webjars "0.2.0" :exclusions [com.fasterxml.jackson.core/jackson-core]]
              [compojure "1.6.1"]
              [hiccup "1.0.5"]
              [hickory "0.7.1"]
              [clj-org "0.0.2"]

              ;; my licence expire on 2017/05/17, upgrade not supported, so only 0.9.5561 supported. can register new account?
              ;; this is free but not for open source
              ;;[com.datomic/datomic-pro "0.9.5561" :exclusions [commons-codec org.apache.httpcomponents/httpclient org.slf4j/slf4j-nop]]
              [com.datomic/datomic-free "0.9.5656" :exclusions [commons-codec org.apache.httpcomponents/httpclient org.slf4j/slf4j-nop com.google.guava/guava]]

              [org.webjars.bower/tether "1.4.4"]
              [org.webjars/bootstrap "4.1.3"]
              [org.webjars.bower/semantic "2.3.1"]
              [endophile "0.2.1"]
              [buddy/buddy-auth "2.1.0"]
              [oauth-clj "0.1.16" :exclusions [commons-logging]]
              [org.danielsz/system "0.4.1"]
              [com.cemerick/pomegranate "1.0.0" :exclusions [commons-logging]]
              [prone "1.6.0"]
              [ring-logger "1.0.1"]

              ;; for tools
              [enlive "1.1.6"]
              [com.draines/postal "2.0.2"]
              ;; for incanter-example
              [net.mikera/core.matrix "0.62.0"]
              ;; core.matrix implementations
              [clatrix "0.5.0"]
              [net.mikera/vectorz-clj "0.48.0"]
              ;; can not get the latest impl now
              [org.clojars.xfcjscn/nd4clj "0.1.2"]
              
              ;; buddy contains a copy for jdk15
              [incanter "1.9.3" :exclusions [org.bouncycastle/bctsp-jdk14 bouncycastle/bcmail-jdk14 bouncycastle/bcprov-jdk14 org.clojure/tools.nrepl]]
              ;; for ml week 4
              [net.mikera/imagez "0.12.0"]
              [org.clojars.xfcjscn/image-matrix "0.1.1"]

              ;; func-plot
              [prismatic/dommy "1.1.0"]
              [hipo "0.5.2"]
              [cljsjs/three "0.0.87-0"]
              ;; browser control, PREREQUISITE: chromedriver to be in $PATH
              [etaoin "0.2.4"]

              [dk.ative/docjure "1.12.0"]
              ]
       :build '[;; for boot(bootstrap)
                [adzerk/boot-cljs "2.1.4"]
                [adzerk/boot-reload "0.5.2"]
                ;; can not use [pandeiro/boot-http "0.8.3"], as internally ring 1.4.0 dependency hardcoded in source
                ;; here boot-alt-http also hard code dependency, but it's ring 1.6.3
                [metosin/boot-alt-http "0.2.0"]
                [adzerk/boot-cljs-repl   "0.3.3"]
                [adzerk/boot-test "1.2.0"]
                [crisptrutski/boot-cljs-test "0.3.4"]
                ]}
      vals
      (->> (apply concat))
      ))


(set-env!
 :resource-paths #{"src" "resources"}
 :dependencies dependencies)

(task-options!
 pom {:project 'apps
      :version "0.1.0-SNAPSHOT"}
 ;; cider don't inject this automaticlly, so duplicate with code in nrepl-cider
 repl {:middleware '[cemerick.piggieback/wrap-cljs-repl]})

(require '[apps.bootstrap.tasks :refer :all])
