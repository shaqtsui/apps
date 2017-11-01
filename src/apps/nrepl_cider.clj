(ns apps.nrepl-cider
  (:require [clojure.tools.nrepl.server :as nrepl-server]
            [cider.nrepl :refer (cider-nrepl-handler cider-middleware)]
            [cemerick.piggieback :as pback]
            ;; currently not used, new func in clojure 1.8
            [clojure.core.server :refer [start-server stop-server]]
            ))

;; cider-nrepl-handler can be hand-tweaked to include cljs middleware
(def cider-nrepl-handler-with-cljs
  "CIDER's nREPL handler."
  (apply nrepl-server/default-handler (map resolve
                                           (conj cider-middleware 'pback/wrap-cljs-repl))))


;; nrepl: message based, support code evaluate, debug... interacts.
;; this only enable nrepl-handler in runtime. normally repl client will be created by cider via emacs program.
;; difference from "lein repl", which will create runtime for you, and a simple client which can not handle cider specific message.
;; but cider will ignore the client, create repl via :headless, and creat a repl client via emacs program.
(defn -main
  []
  (nrepl-server/start-server :port 7888 :handler cider-nrepl-handler-with-cljs))


;; socket repl: streem based(read & write string from stream), only support code evaluate interact.
(defn start-socket-server-for-repl
  []
  (start-server {:name "repl" :port 5555 :accept 'clojure.core.server/repl :address "0.0.0.0" :server-daemon false}))
