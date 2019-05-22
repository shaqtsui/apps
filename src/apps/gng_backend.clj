(ns apps.gng-backend
  (:require [apps.ring-mw :as mw]
            [buddy.auth :as auth]
            [buddy.auth.accessrules :as auth-ac]
            [clojure.java.jdbc :as j]
            [compojure.core :as cpj]
            [compojure.route :as cpj-r]
            [cprop.source :as cps]
            [datomic.api :as d]
            [hikari-cp.core :as hcp]
            [hugsql.core :as hugsql]
            [immutant.web :as web]
            [migratus.core :as migratus]
            [mount.core :as mnt]
            [prone.debug :as pd]
            [ring.util.response :as resp]
            [struct.core :as st]
            [taoensso.timbre :as timbre]
            [taoensso.timbre.appenders.core :as appenders]))










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

(defn to-clj-data
  "PgArray -> Array -> seq
  PgArray cannot be encoded by muuntaja format application/json
  Array cannot be encoded by muuntaja format application/transit+json
  "
  [ms key]
  (map #(update % key
                (comp seq
                      (memfn getArray)))
       ms))

;; session work when in same site
(def app
  (-> (cpj/routes
       (cpj/POST "/hello" request
                 (resp/response (merge {:r1 1
                                        "r2" 2}
                                       (:body-params request))))
       (cpj/POST "/login" request
                 (let [p (-> request
                             timbre/spy
                             #_:params
                             :body-params)
                       op (folk-by-name db-spec p)]
                   (if (nil? op)
                     (do
                       (insert-folk db-spec p)
                       (-> {:status :ok}
                           resp/response
                           (assoc :session
                            (-> request :session
                                (assoc :identity "foo")))
                           timbre/spy))
                     (resp/response {:status :error
                                     :message "name exist"}))
                   ))
       (cpj/GET "/folk" request
                (resp/response (folk-by-name db-spec (:params request))))
       (cpj/GET "/item" request
                (-> db-spec
                    items
                    (to-clj-data :img_urls)
                    resp/response))
       (cpj-r/not-found "page not found"))
      (auth-ac/wrap-access-rules {:rules [{:uri "/folk"
                                           :handler auth/authenticated?}]
                                  :on-error (fn [req val]
                                              {:status 200
                                               :body (str "Access to: " (:uri req) " is not authorized")})})
      mw/wrap-defaults))

(mnt/defstate server
  :start (web/run app {:port 8080})
  :stop (web/stop))


(mnt/start)

#_(mnt/stop)

#_(items db-spec)

(def migration-options {:store :database
                        :migration-dir "apps/gng_migrations"
                        :db db-spec})

(migratus/migrate migration-options)



;;(migratus/rollback migration-options)

;;(migratus/create migration-options "data")


;;(str/trim "dd")





