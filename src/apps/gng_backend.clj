(ns apps.gng-backend
  (:require [datomic.api :as d]
            [aleph.http :as http]
            [apps.ring-mw :as mw]
            [mount.core :as mnt])
  (:import io.netty.handler.logging.LoggingHandler
           io.netty.handler.logging.LogLevel))


(def hello-world-handler
  (mw/wrap-defaults
   (fn [req]
     {:status 200
      :headers {"content-type" "text/plain"}
      :body "Hello World"})))


(defn add-log [pl]
  (.addLast pl
            (LoggingHandler. LogLevel/DEBUG)))


(def db-uri "datomic:mem://localhost:4334/gng")

(mnt/defstate db-conn
  :start (do (d/create-database db-uri)
             (d/connect db-uri))
  :stop (d/release db-conn))

(mnt/defstate server
  :start (http/start-server hello-world-handler {:port 8080
                                                 :pipeline-transform add-log})
  :stop (.close server))

(mnt/start)


#_(mnt/stop)


