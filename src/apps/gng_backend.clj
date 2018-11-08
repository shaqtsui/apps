(ns apps.gng-backend
  (:require [datomic.api :as d]
            [aleph.http :as http]
            [apps.ring-mw :as mw]
            [mount.core :as mnt]
            [cprop.core :as prp]
            [clojure.java.jdbc :as j]
            [hikari-cp.core :as hcp]
            [hugsql.core :as hugsql])
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



(def env (prp/load-config))

(mnt/defstate db-conn
  :start (do (d/create-database (env :db-url))
             (d/connect (env :db-url)))
  :stop (d/release db-conn))




(mnt/defstate server
  :start (http/start-server hello-world-handler {:port 8080
                                                 :pipeline-transform add-log})
  :stop (.close server))



(mnt/start)


#_(mnt/stop)



(hugsql/def-db-fns "apps/gng.sql")



(def db-spec {:datasource (hcp/make-datasource (env :datasource-options))})

;;(create-items-table db-spec)

(item-by-id db-spec {:id 2})

(insert-item db-spec {:name "iPhone" :detail "iphone SE"})

