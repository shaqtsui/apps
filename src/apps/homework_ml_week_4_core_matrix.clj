(ns apps.homework-ml-week-4-core-matrix
  (:require [clojure.tools.logging :as logging]
            [clojure.tools.reader :as r]
            [clojure.java.io :as io]
            [clojure.core.matrix :as m]
            [clojure.core.matrix.stats :as m-s]
            [clojure.string]
            [incanter.core :as i]
            [incanter.charts :as i-c]
            [apps.ml-for-core-matrix :refer :all]
            [apps.image :refer :all]
            [mikera.image.core :as img]))

(m/set-current-implementation :vectorz)

(as-> "homework_ml_week_4/ex3data1_x.csv" $
      (io/resource $)
      (io/reader $)
      (line-seq $)
      (map #(clojure.string/split % #",") $)
      (m/emap r/read-string $)
      (def ex3-data-1-x $))


(as-> "homework_ml_week_4/ex3data1_y.csv" $
      (io/resource $)
      (io/reader $)
      (line-seq $)
      (map #(clojure.string/split % #",") $)
      (m/emap r/read-string $)
      (def y $))

(def x-imgs
  (convert-to-img ex3-data-1-x (m/emin ex3-data-1-x) (m/emax ex3-data-1-x)))

(-> x-imgs
    (nth 1000)
    img/show)

(-> x-imgs
    (merge-imgs 100)
    img/show)



(def x
  (map-feature ex3-data-1-x 1))

(def lamb (-> (repeat (- (m/column-count x) 1) 0.1)
              (conj 0)
              (m/div (m/row-count x))
              (m/div 2)))

(def init-ts
  (repeat (count (distinct y)) (init-theta x)))

;;
;;#_(def init-ts
;;  best-theta)
;;
;;
(def thetas (map  (fn [c init-t] (-> #(gradient-desent x 
                                                       (to-y-class y c)
                                                       %
                                                       lamb
                                                       4
                                                       1)
                                     (iterate init-t)
                                     ((partial take 5))))
                  (distinct y)
                  init-ts))

(def mean-costs (map (fn [ts c]
                       (map
                         (fn [t]
                           (-> (get-cost x (to-y-class y c) t lamb)
                               m-s/mean))
                         ts))
                     thetas
                     (distinct y)))

(map #(-> (i-c/line-chart (range 20) %)
          i/view)
     mean-costs)

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
