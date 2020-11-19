(ns apps.noioc
  (:require [ajax.core :as aj]
            [mount.core :as mnt]
            [compojure.core :as cpj]
            [ring.util.response :as resp]
            [immutant.web :as web]
            [apps.ring-mw :as mw]
            [clojure.java.io :as io]
            [taoensso.timbre :as timbre]
            ))

#_(timbre/set-level! :debug)


(def wrapped-funcs
  "uuid -> plain function"
  (atom {}))

(defn relay-spell
  "relay '(apply f [args]"
  [f-uuid & args]
  (-> wrapped-funcs
      deref
      (apply [f-uuid])
      (apply args)))


(defn recieve-spell
  "receive on port 10080, re-execute this function will update app
  see: my-cognition/immutang web"
  []
  (let [app (mw/wrap-defaults (cpj/routes
                               (cpj/POST "/func" request
                                         (let [body-params (:body-params request)
                                               f-uuid (:id body-params)
                                               args (:args body-params)
                                               result (apply relay-spell f-uuid args)]
                                           (resp/response {:result result})))))]
    (web/run app {:port 10080})))

(def server (recieve-spell))

(defn spell-at-remote
  "spell '(apply f [args]) at remote
  web impl boundry
  "
  [server f-uuid & args]
  (let [p (promise)]
    (aj/POST (format "http://%s:%d%s%s"
                     (:host server)
                     (:port server)
                     (:path server)
                     (if (clojure.string/ends-with? (:path server) "/")
                       "func"
                       "/func"))
             {:params {:id f-uuid
                       :args args}
              ;; by default
              ;; :response-format :json
              :handler #(deliver p (:result %))
              :error-handler #(deliver p %)})
    @p))

(defn wrap-web
  "save mapping: uuid -> f in wrapped-funcs
  return function perform ajax call with :params/:id = uuid
  "
  [f]
  (let [uuid (java.util.UUID/randomUUID)]
    (swap! wrapped-funcs assoc uuid f)
    (partial spell-at-remote server uuid)))


;; example
#_(-> (fn [name value]
        (str "Good: " value " my name: " name))
      wrap-web
      (apply [1 2]))
