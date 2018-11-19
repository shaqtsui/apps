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
            [compojure.route :as cpj-r]
            [ring.util.response :as resp]
            [muuntaja.middleware :as mw-mu]
            [buddy.auth :as auth]
            [buddy.auth.accessrules :as auth-ac]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as st]
            [clojure.spec.gen.alpha :as sg]

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



(s/valid? even? 10)

(s/conform even? 1)

;; registry to a namespaced keyword
;; s/def better rename to s/bind!
(s/def ::name (s/and string? #(> (count %) 2)))

(s/valid? ::name "123")

(s/explain ::name 1)


(s/def ::folk (s/keys :req [::name]
                      :opt [::password]))

(s/def ::vip (s/merge ::folk
                      (s/keys :req [::star])))

(s/conform ::folk {::name "ddd"
                   ::password "dd"
                   ::age 13})

(s/explain ::folk {
                   ::password "dd"
                   ::age 13})

(s/def :unq/folk (s/keys :req-un [::name]))

(s/explain :unq/folk {:name "d"})

;; error
#_(s/def :unfolk (s/keys :req-un [::name]))


(s/explain ::vip {::name "ddd"
                  ::star 12})

(s/explain ::vip {::name "ddd"})

(s/conform (s/coll-of keyword?)
           [:a :b 3])

(s/conform (s/coll-of number?)
           #{1 2 "d"})


(s/def ::vnum3 (s/coll-of number? :kind vector? :count 3 :distinct true))

(s/conform ::vnum3 #{1 2 3})

(s/conform ::vnum3 [1 2 3])


(s/def ::point (s/tuple double? double? double?))


(s/conform ::point [1 2 3])

(s/conform ::point [1.1 2.2 3.3])


(s/def ::scores (s/map-of string? int?))

(s/conform ::scores {"dd" 1})

(s/conform ::scores {:dd 1})

(s/def ::ingredient (s/cat :quant number?
                           :unit keyword?))

(s/conform ::ingredient [2 :dd])

(s/conform ::ingredient [2 :dd 3 :cc])

(s/def ::seq-of-kw (s/* keyword?))

(s/conform ::seq-of-kw [1 2])

(s/conform ::seq-of-kw [:1 :3])


(s/def ::odd-may-even (s/cat :odds (s/+ odd?)
                             :even (s/? even?)))


(s/conform ::odd-may-even [1 3])
(s/conform ::odd-may-even [1 3 2 2])
(s/conform ::odd-may-even [1 3 2])

(s/conform ::odd-may-even [])

(s/conform ::odd-may-even [10])



(s/def ::config (s/* (s/cat :p string?
                            :v (s/alt :s string?
                                      :b boolean?))))

(s/explain ::config ["server" true "port" "09"])

(s/explain ::config ["server" true "port" :90])


(s/describe ::config)

(s/def ::even-strings (s/& (s/* string?)
                           #(even? (count %))))


(s/explain ::even-strings ["a" "b"])

(s/explain ::even-strings ["a" "b" "c"])

(s/def ::nested
  (s/cat :n-kw #{:names}
         :names (s/spec (s/* string?))
         :nu-kw #{:nums}
         :nums (s/spec (s/* number?))))


(s/explain ::nested [:names ["a" "b"]
                     :nums [1 2]])

(s/explain ::nested [:names ["a" "b"]
                     :nums [1 2 ""]])



(::name {::name "ddd"})



(s/fdef ttt
  :args (s/and (s/cat :start int?
                      :end int?)
               #(< (:start %) (:end %)))
  :ret int?)

(defn  ttt [start end]
  (+ start end))



;; rely on spes, so need to pass fully-qualified symbol
;; instrument func to add spec corresponding validation
(st/instrument `ttt)
(ttt 1 2)
(ttt 2 1)
(clojure.pprint/pprint (st/check `ttt))
(s/exercise-fn `ttt)



;; generater
(sg/generate (s/gen int?))

(sg/sample (s/gen int?))



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
       (cpj-r/not-found "page not found"))
      (auth-ac/wrap-access-rules {:rules [{:uri "/folk"
                                           :handler auth/authenticated?}]
                                  :on-error (fn [req val]
                                              {:status 200
                                               :body (str "Access to: " (:uri req) " is not authorized")})})
      mw/wrap-defaults))



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
