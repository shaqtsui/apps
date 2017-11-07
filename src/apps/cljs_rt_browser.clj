(ns apps.cljs-rt-browser
  (:require [clojure.tools.logging :as log]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [compojure.core :refer [GET routes]]
            [compojure.route :as route]
            [hiccup.core :refer [html]]
            [hiccup.page :refer [include-js]]
            [hiccup.element :refer [javascript-tag]]
            [com.stuartsierra.component :as component]
            [system.repl :refer [set-init! start stop reset]]
            [system.components.jetty :refer [new-web-server]]
            cljs.build.api))

(def repl-client-url "http://192.168.124.5:9000/repl")

(def index-hcp
  [:html
   [:head]
   [:body
    (include-js "goog/base.js" "main.js")
    (javascript-tag (cljs.build.api/compile {} '(require 'clojure.browser.repl)))
    (javascript-tag (cljs.build.api/compile {} (apply list `(clojure.browser.repl/connect ~repl-client-url))))]])

(def app-routes
  (routes
   (GET "/" request (html index-hcp))
   (route/not-found "Page Not Found")))

;; this must be no parameter so it can be invoked by system/init
(defn system-map-creator []
  (component/system-map
   ;; in linux port below 1024 can only be opened by root
   :web (new-web-server 8080 (wrap-defaults app-routes
                                            (assoc-in site-defaults [:static :files] "out")))))


(defn -main
  "Start the application"
  []
  (set-init! #'system-map-creator)
  (start))


#_(-main)

#_(reset)

#_(stop)
