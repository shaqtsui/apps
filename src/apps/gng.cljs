(ns apps.gng
  (:require [reagent.core :as r]
            [dommy.core :as dommy]
            [cljsjs.material-ui]
            [cljs-react-material-ui.core :refer [create-mui-theme color]]
            [cljs-react-material-ui.reagent :as ui]
            [cljs-react-material-ui.icons :as ic]
            [ajax.core :as aj]))



(aj/GET "http://localhost:8080/hello")

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


(r/render [main]
          (dommy/sel1 :#app))
