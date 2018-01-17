(set-env!
 :resource-paths #{"src"}
 :dependencies '[[org.clojure/clojure "1.9.0"]
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
                 ;; for flycheck-clojure 
                 [acyclic/squiggly-clojure "0.1.9-SNAPSHOT"]

                 ;; cms
                 [ring "1.6.3"]
                 [ring/ring-defaults "0.3.1"]
                 [ring-webjars "0.2.0" :exclusions [com.fasterxml.jackson.core/jackson-core]]
                 [compojure "1.6.0"]
                 [hiccup "1.0.5"]
                 [hickory "0.7.1"]
                 
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
                 [net.mikera/core.matrix "0.61.0"]
                 [incanter "1.9.0"]
                 ;; for ml week 4
                 [net.mikera/imagez "0.12.0"]

                 [quil "2.6.0"]
                 ;; mvn install:install-file -DgroupId=apps -DartifactId=extruder -Dversion=1.02 -Dpackaging=jar -Dfile=extruder.jar -DgeneratePom=true -DcreateChecksum=true
                 [apps/extruder "1.02"]
                 [apps/peasycam "202"]
                 [apps/shapes3d "2.2"]

                 ;; func-plot
                 [prismatic/dommy "1.1.0"]
                 [hipo "0.5.2"]
                 [cljsjs/three "0.0.87-0"]

                 ;; sources
                 [org.processing/core "3.3.6" :classifier "sources"]

                 ])

(task-options!
 pom {:project 'apps
      :version "0.1.0-SNAPSHOT"}
 ;; cider don't inject this automaticlly, so duplicate with code in nrepl-cider
 repl {:middleware '[cemerick.piggieback/wrap-cljs-repl]}
 )

