(ns apps.ring-mw
  (:require [buddy.auth.backends :as backends]
            ;; prone.middleware have more info than ring.middleware.stacktrace
            [prone.middleware :as prone]
            [ring.logger :as logger]
            [ring.middleware.reload :as mw-r]
            [ring.middleware.defaults :as mw-def]
            [ring.middleware.webjars :as mw-wj]
            [buddy.auth.middleware :as mw-au]
))


(defn wrap-history
  "Get a list of history urls via:
     (-> response
       :session
       :history)"
  [handler & {:keys [max-size ignore-4xx? ignore-5xx?]
              :or {max-size 10}}]
  (fn [request]
    (let [response (handler request)]
      (if (or (zero? max-size)
              (and ignore-4xx?
                   (> (:status response) 399)
                   (< (:status response) 500))
              (and ignore-5xx?
                   (> (:status response) 499)
                   (< (:status response) 600)))
        response
        (->  (if (contains? response :session)
               response
               (assoc response :session (:session request)))
             (update-in [:session :history]
                        (fn [history]
                          (as-> (or history []) $$
                            (if (or (< max-size 0) (> max-size (count $$)))
                              $$
                              (subvec $$ 1 max-size))
                            (conj $$ (:uri request))))))))))

(defn wrap-defaults
  "Default middleware used by all apps.*"
  [handler]
  (let [auth-backend (backends/session)]
    (-> handler
        (wrap-history)
        (mw-au/wrap-authorization auth-backend)
        (mw-au/wrap-authentication auth-backend)
        (prone/wrap-exceptions {:app-namespaces '[apps]})
        (logger/wrap-with-logger {:printer :no-color})
        mw-wj/wrap-webjars
        (mw-def/wrap-defaults (-> mw-def/site-defaults
                           (assoc-in [:security :frame-options] {:allow-from "http://localhost:8080"})
                           (assoc-in [:security :anti-forgery] false)))
        mw-r/wrap-reload)))

