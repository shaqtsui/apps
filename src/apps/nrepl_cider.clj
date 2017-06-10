(ns apps.nrepl-cider
  (:require [clojure.tools.nrepl.server :as nrepl-server]
            [cider.nrepl :refer (cider-nrepl-handler)]
            ;; currently not used, new func in clojure 1.8
            [clojure.core.server :refer [start-server stop-server]]

            ))

(defn -main
  []
  (nrepl-server/start-server :port 7888 :handler cider-nrepl-handler))


(defn start-socket-server-for-repl
  []
  (start-server {:name "repl" :port 5555 :accept 'clojure.core.server/repl :address "0.0.0.0" :server-daemon false}))
