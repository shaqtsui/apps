(ns apps.homework-ml-week-4-newton
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
            [apps.matrix :as a-m]
            [mikera.image.core :as img]))

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

#_(def best-theta (a-m/search-convergence-point
                 (fn [theta]
                   (->
                    (get-cost x (to-y-class y 1) % lamb)
                    m-s/mean))
                 (init-theta x)))

