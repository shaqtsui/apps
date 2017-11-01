(ns apps.hello-world
  (:require [clojure.browser.repl :as repl]))

(enable-console-print!)

(repl/connect "http://192.168.124.4:9000/repl")

(println "Hello world")


