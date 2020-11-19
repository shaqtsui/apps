(ns apps.noioc
  (:require [ajax.core :as aj]
            #?@(:cljs [[dommy.core :as dommy]
                       [reagent.core :as reagent]
                       [reagent.dom :as reagentd]
                       [reagent-material-ui.components :as ui]
                       [reagent-material-ui.colors :as colors]
                       [reagent-material-ui.styles :as styles]])
            ))


#?(:cljs (enable-console-print!))

(println "hellow....")

#?(:cljs (defn home []
           (let [state (reagent/atom {})]
             #_(aj/GET "http://localhost:8080/item" {:handler #(swap! state assoc :items %)})
             (fn []
               ;; insert style in header?
               [styles/theme-provider (styles/create-mui-theme {:palette {:primary   colors/purple
                                                                          :secondary colors/green}})
                [:div
                 [ui/toolbar
                  [ui/button "subscripe"]
                  [ui/icon-button "test"]
                  [ui/button "sign up"]]
                 [ui/paper
                  "This is the home page"]
                 [ui/grid {:container true}
                  (map (fn [item]
                         [ui/grid {:item true
                                   :md 3
                                   :key (:id item)}
                          [ui/card
                           [ui/card-header {:title  (:name item)
                                            :subheader "This is subheader"
                                            }]
                           [ui/card-media {:image (-> item :img_urls first)}]
                           [ui/card-content (:detail item)]]])
                       (:items @state))]
                 ]]))))

#?(:cljs (reagentd/render [home]
                          (dommy/sel1 :#app)))

