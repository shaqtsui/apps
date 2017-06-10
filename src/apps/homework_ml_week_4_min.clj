(ns apps.homework-ml-week-4-min
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

(def x
  (map-feature ex3-data-1-x 1))

(def lamb (-> (repeat (- (ncol x) 1) 0.1)
              (conj 0)
              (div (nrow x))
              (div 2)))


(defn min-cost [c] 
  (minimize (fn [t]
              (as-> (mean (get-cost x (to-y-class y c) t lamb)) $
                    (if (Double/isNaN $)
                      (logging/spy $)
                      $)))
            (repeat
              (ncol x)
              0.01)))


(def result (map min-cost (distinct y)))


(map (fn [r]
       (as-> r $
             (:value $)
             )) 
     result)



(def predicted-y-chances (apply bind-columns (map (fn [r]
                                                    (-> r
                                                        :value
                                                        get-hypo-func
                                                        (get-y-hat x)))
                                                  result)))

(def best-y (map (fn [y-rates]
                   (as-> (zipmap y-rates (distinct y)) $
                         (find $ (apply max (keys $)))))
                 predicted-y-chances))
best-y
(accuracy (map last best-y) y)





