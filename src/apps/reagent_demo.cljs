(ns apps.reagent-demo
  (:require [reagent.core :as r]
            [dommy.core :as dommy]
            [cljsjs.material-ui]
            [cljs-react-material-ui.core :refer [create-mui-theme color]]
            [cljs-react-material-ui.reagent :as ui]))

(defn comp-1 []
  [:div
   [:h3 "I am comp 1"]])


(def app-state (r/atom 0))

(defn comp-2 []
  [:div {:on-click #(swap! app-state inc)}
   "I have been clicked " @app-state " times"])


(def base-theme (create-mui-theme {:palette {:primary {:main (color :light-blue 700)}
                                                              :secondary {:main (color :teal :A100)}
                                                              :text-color (color :common :white)}}))

(defn comp-3 []
  [ui/mui-theme-provider {:theme base-theme}
   [ui/button "Click Me"]])

(r/render [comp-3] (dommy/sel1 :#app))
