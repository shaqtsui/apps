(ns apps.gng-backend
  (:require [datomic.api :as d]
            [aleph.http :as http]
            [apps.ring-mw :as mw]
            [mount.core :as mnt]
            [cprop.core :as prp]
            [clojure.java.jdbc :as j]
            [hikari-cp.core :as hcp]
            [hugsql.core :as hugsql]
            [migratus.core :as migratus])
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



(def env (prp/load-config :resource "apps/gng_config.edn"))

(mnt/defstate db-conn
  :start (do (d/create-database (env :db-url))
             (d/connect (env :db-url)))
  :stop (d/release db-conn))



(mnt/defstate server
  :start (http/start-server hello-world-handler {:port 8080
                                                 :pipeline-transform add-log})
  :stop (.close server))


(mnt/defstate db-spec
  :start {:datasource (hcp/make-datasource (env :datasource-options))}
  :stop (-> db-spec
            :datasource
            hcp/close-datasource))

(mnt/start)


#_(mnt/stop)



(hugsql/def-db-fns "apps/gng.sql")


(item-by-id db-spec {:id 2})

(insert-item db-spec {:name "iPhone" :detail "iphone SE"})

(def migration-options {:store :database
                       :migration-dir "apps/gng_migrations"
                       :db db-spec})

(migratus/migrate migration-options)
(migratus/rollback migration-options)

;;(migratus/create migration-options "create-user")

