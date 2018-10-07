(ns apps.gng-backend
  (:require [datomic.api :as d]
            [aleph.http :as http]
            [mount.core :as mnt]))


(defn hello-world-handler [req]
  {:status 200
   :headers {"content-type" "text/plain"}
   :body "Hello World"})







(def db-uri "datomic:mem://localhost:4334/gng")

(mnt/defstate db-conn
  :start (do (d/create-database db-uri)
             (d/connect db-uri))
  :stop (d/release db-conn))

(mnt/defstate server
  :start (http/start-server hello-world-handler {:port 8080})
  :stop (.close server))

(mnt/start)





