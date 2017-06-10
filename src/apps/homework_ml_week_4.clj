(ns apps.homework-ml-week-4
  (:require [clojure.tools.logging :as logging]
            [clojure.java.io :as io]
            [apps.ml :refer :all]
            [apps.image :refer :all]
            [incanter.core :refer :all]
            [incanter.stats :refer :all]
            [incanter.charts :refer :all]
            [incanter.io :refer :all]
            [incanter.datasets :refer :all]
            [incanter.pdf :refer :all]
            [incanter.optimize :refer :all]
            [incanter.latex :refer :all]
            [mikera.image.core :as img]))

#_(require '[apps.ml :refer :all] :reload)

(def ex3-data-1-x
  (-> "homework_ml_week_4/ex3data1_x.csv"
      io/resource
      read-dataset
      to-matrix))

(def y
  (-> "homework_ml_week_4/ex3data1_y.csv"
      io/resource
      read-dataset
      to-matrix))

(def img-max-grayscale (as-> (map #(apply max %) ex3-data-1-x) $
                             (apply max $)))
(def img-min-grayscale (as-> (map #(apply min %) ex3-data-1-x) $
                             (apply min $)))
(def x-imgs
  (convert-to-img ex3-data-1-x img-min-grayscale img-max-grayscale))

#_(-> x-imgs
    (nth 1000)
    img/show)

#_(-> x-imgs
    (merge-imgs 100)
    img/show)
 
;;(def x
;;  (map-feature ex3-data-1-x 1))
;;
;;(def lamb (-> (repeat (- (ncol x) 1) 0.1)
;;              (conj 0)
;;              (div (nrow x))
;;              (div 2)))
;;
;;(def init-ts
;;  (repeat (count (distinct y)) (init-theta x)))
;;
;;
;;#_(def init-ts
;;  best-theta)
;;
;;
;;(def thetas (map  (fn [c init-t] (-> #(gradient-desent x 
;;                                                (to-y-class y c)
;;                                                %
;;                                                lamb
;;                                                4
;;                                                1)
;;                              (iterate init-t)
;;                              ((partial take 20))))
;;                  (distinct y)
;;                  init-ts))
;;
;;
;;(def mean-costs (map (fn [ts c]
;;                       (map
;;                         (fn [t]
;;                           (-> (get-cost x (to-y-class y c) t lamb)
;;                               mean))
;;                         ts))
;;                     thetas
;;                     (distinct y)))
;;
;;
;;(map #(-> (line-chart (range 20) %)
;;          view)
;;     mean-costs)
;;
;;
;;(def best-theta (map last thetas))
;;(def best-mean-cost (map last mean-costs))
;;
;;(def predicted-y-chances (apply bind-columns (map (fn [t]
;;                                                    (-> t
;;                                                        get-hypo-func
;;                                                        (get-y-hat x)))
;;                                                  best-theta)))
;;
;;(def best-y (map (fn [y-rates]
;;                   (as-> (zipmap y-rates (distinct y)) $
;;                         (find $ (apply max (keys $)))))
;;                 predicted-y-chances))
;;best-y
;;(accuracy (map last best-y) y)
;;
;;
