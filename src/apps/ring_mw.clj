(ns apps.ring-mw
  (:require [buddy.auth.backends :as backends]
            ;; prone.middleware have more info than ring.middleware.stacktrace
            [prone.middleware :as prone]
            [ring.logger :as logger]
            [ring.middleware.reload :as mw-r]
            [ring.middleware.defaults :as mw-def]
            [ring.middleware.webjars :as mw-wj]
            [buddy.auth.middleware :as mw-au]
            [muuntaja.middleware :as mw-mu]
            [ring.middleware.cors :as mw-cors]
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
        wrap-history
        (mw-au/wrap-authorization auth-backend)
        (mw-au/wrap-authentication auth-backend)
        mw-mu/wrap-format
        (prone/wrap-exceptions {:app-namespaces '[apps]})
        (logger/wrap-with-logger {:printer :no-color})
        mw-wj/wrap-webjars
        (mw-cors/wrap-cors :access-control-allow-origin [#".*"]
                           :access-control-allow-methods [:get :put :post :delete])
        (mw-def/wrap-defaults (-> mw-def/site-defaults
                           (assoc-in [:security :frame-options] {:allow-from "*"})
                           (assoc-in [:security :anti-forgery] false)))
        ;; bug: inside server to stop & start external server when reload ns cause server state restart
        ;; track source file & reload it, wrap-reload follow implicit over explicit
        ;; use emacs C-k to explict reload, states will be restarted by mount
        #_mw-r/wrap-reload)))

