(ns apps.gng-backend
  (:require [datomic.api :as d]
            [aleph.http :as http]
            [immutant.web :as web]
            [apps.ring-mw :as mw]
            [mount.core :as mnt]
            [cprop.source :as cps]
            [clojure.java.jdbc :as j]
            [hikari-cp.core :as hcp]
            [hugsql.core :as hugsql]
            [prone.debug :as pd]
            [migratus.core :as migratus]
            [taoensso.timbre :as timbre] ;; implicit require macro
            [taoensso.timbre.appenders.core :as appenders]
            [compojure.core :as cpj]
            [ring.util.response :as resp]
            [muuntaja.middleware :as mw-mu]
            )
  (:import io.netty.handler.logging.LoggingHandler
           io.netty.handler.logging.LogLevel))

;; current config: (clojure.pprint/pprint timbre/*config*)
;; alternative env setup:  export TIMBRE_LEVEL=':trace'
;; alternative simple level change: (timbre/set-level! :trace)
(timbre/merge-config!
 {:level :debug
  ;;:appenders {:spit (appenders/spit-appender {:fname "apps.log"})}
  })

(def env (-> {:db-url "datomic:mem://localhost:4334/gng"
              :datasource-options {:auto-commit        true
                                   :read-only          false
                                   :connection-timeout 30000
                                   :validation-timeout 5000
                                   :idle-timeout       600000
                                   :max-lifetime       1800000
                                   :minimum-idle       10
                                   :maximum-pool-size  10
                                   :pool-name          "db-pool"
                                   :adapter            "postgresql"
                                   :username           "fuchengxu"
                                   :password           "password"
                                   :database-name      "fuchengxu"
                                   :server-name        "localhost"
                                   :port-number        5432
                                   :register-mbeans    false}}
             (merge (:gng (cps/from-system-props)))))


#_(mnt/defstate db-conn
  :start (do (d/create-database (env :db-url))
             (d/connect (env :db-url)))
  :stop (d/release db-conn))

(mnt/defstate db-spec
  :start {:datasource (hcp/make-datasource (env :datasource-options))}
  :stop (-> db-spec
            :datasource
            hcp/close-datasource))





(hugsql/def-db-fns "apps/gng.sql")

(def app
  (mw/wrap-defaults (cpj/routes
                     (cpj/POST "/hello" request
                                (resp/response (merge {:r1 1
                                                      "r2" 2}
                                                     (:body-params request))))
                     (cpj/POST "/login" request
                               (let [p (-> request
                                           timbre/spy
                                           :params)
                                     op (folk-by-name db-spec p)]
                                 (if (nil? op)
                                   (do
                                     (insert-folk db-spec p)
                                     {:status :ok})
                                   {:status :error
                                    :message "name exist"})
                                 )))))



(defn add-log [pl]
  (.addLast pl
            (LoggingHandler. LogLevel/DEBUG)))


#_(mnt/defstate server
    :start (http/start-server hello-world-handler {:port 8080
                                                   ;;:pipeline-transform add-log
                                                   })
    :stop (.close server))

(mnt/defstate server
  :start (web/run app {:port 8080})
  :stop (web/stop))


(mnt/start)


#_(mnt/stop)




(def migration-options {:store :database
                        :migration-dir "apps/gng_migrations"
                        :db db-spec})

(migratus/migrate migration-options)


;;(migratus/rollback migration-options)


;;(migratus/create migration-options "create-user")
