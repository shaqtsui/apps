(ns apps.gng
  (:require [reagent.core :as r]
            [dommy.core :as dommy]
            [cljsjs.material-ui]
            [cljs-react-material-ui.core :refer [create-mui-theme color]]
            [cljs-react-material-ui.reagent :as ui]
            [cljs-react-material-ui.icons :as ic]
            [ajax.core :as aj]
            [secretary.core :as secretary]
            [goog.events :as events]
            [goog.history.EventType :as HistoryEventType]
            [struct.core :as st]
            )
  (:import goog.History))


(secretary/set-config! :prefix "#")
(secretary/defroute "/folks/:id" {:as params}
  (js/console.log (str "user: " (:id params))))

(secretary/dispatch! "/folks/sh")


(defn hook-browser-navigation! []
  (doto (History.)
    (#_events/listen dommy/listen! HistoryEventType/NAVIGATE
                   #(secretary/dispatch! (.-token %)))
    (.setEnabled true)))

;; will hang the browser
;;(hook-browser-navigation!)


(def folk-schema
  [[:name st/required st/string]
   [:password st/required st/string]])


(st/validate {:name "Shark" :password 2} folk-schema)
(st/valid? {:name "Shark" :password "dd"} folk-schema)


(aj/POST "http://localhost:8080/hello" {:handler #(println %)
                                        :params {:p1 "p1"
                                                 :p2 "p2"}})
(aj/GET "http://localhost:8080/ddddd" {:handler #(println %)})
(aj/GET "http://localhost:8080/folk" {:handler #(println %)
                                      :params {:name "T9"}})


(aj/POST "http://localhost:8080/login" {:handler #(println %)
                                        :params {:name "T19"
                                                 :password "T"}})


(def base-theme (create-mui-theme {:palette {:primary {:main (color :light-blue 700)}
                                                              :secondary {:main (color :teal :A100)}
                                             :text-color (color :common :white)}}))

(defn main []
  (let [drawer-state (r/atom false)]
    (fn []
      [ui/mui-theme-provider {:theme base-theme}
       [:div
        [ui/app-bar
         [ui/toolbar
          [ui/icon-button {:on-click #(swap! drawer-state not)}
           [ic/menu]]
          [ui/typography {:style {:flex-grow "1"}}
           "GNG"]
          [ui/icon-button
           [ic/account-circle]]]]
        [ui/drawer {:open @drawer-state}
         [ui/icon-button
          [ic/chevron-left {:on-click #(swap! drawer-state not)}]]
         [ui/list
          [ui/list-item
           [ui/list-item-text "All Items"]]]]
        [ui/grid
         "ddddd"]]])))

(defn login []
  (let [state (r/atom nil)]
    (fn []
      [ui/mui-theme-provider {:theme base-theme}
       [:form {:action "http://localhost:8080/login"
               :method :post}
        [ui/form-group
         [ui/text-field {:label "Name"
                         :name "name"}]
         [ui/text-field {:label "Password"
                         :type :password
                         :name "password"}]

         [ui/button {:type :submit}
          "Submit"]]
        ]])))


(r/render [login]
          (dommy/sel1 :#app))

