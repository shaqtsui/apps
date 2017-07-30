(ns apps.homework-ml-week-3-newton
  (:require [clojure.tools.logging :as logging]
            [clojure.core.matrix :as m]
            [clojure.core.matrix.stats :as m-s]
            [clojure.tools.reader :as r]
            [clojure.java.io :as io]
            [apps.matrix :as a-m]
            [apps.ml-for-core-matrix :refer :all]
            [incanter.core :refer [view]]
            [incanter.charts :as i-c]))



(as-> "homework_ml_week_3/ex2data1.txt" $
      (io/resource $)
      (io/reader $)
      (line-seq $)
      (map #(clojure.string/split % #",") $)
      (m/emap r/read-string $)
      (def ex2-data-1 $))

(defn predict-new-data [theta x]
  (-> theta
      get-hypo-func
      (get-y-hat x)
      ((partial map #(if (>= % 0.5) 1 0)))))

(def x-no-intercept (m/select ex2-data-1 :all [0 1]))
(def y (m/select ex2-data-1 :all 2))
(def x (map-feature x-no-intercept 1))

(def lamb (-> (repeat (m/column-count x) 0.1)))

(def best-theta
  (a-m/search-convergence-point
   (fn [theta]
     (-> (get-cost x y theta lamb)
         m-s/mean))
   (init-theta x)))






(as-> "homework_ml_week_3/ex2data2.txt" $
      (io/resource $)
      (io/reader $)
      (line-seq $)
      (map #(clojure.string/split % #",") $)
      (m/emap r/read-string $)
      (def ex2-data-2 $))

(def x-no-intercept (m/select ex2-data-2 :all [0 1]))
(def y (m/select ex2-data-2 :all 2))
(def x (map-feature x-no-intercept 6))

(def data-plot (i-c/scatter-plot (m/select x-no-intercept :all 0) (m/select x-no-intercept :all 1) :group-by y))
(view data-plot)

(def lamb (-> (repeat (- (m/column-count x) 1) 0)
              (conj 0)
              (m/div (m/row-count x))
              (m/div 2)))
(def convergence-point
  (a-m/search-convergence-point
   (fn [theta]
     (-> (get-cost x y theta lamb)
         m-s/mean))
   (init-theta x)))


(def best-theta (:X best-theta))

(accuracy (predict-new-data best-theta x) y)

#_(def u (-> (range -1 1.5 (/ 2.5 500))))

#_(def x-y-space (get-x-y-space u u (fn [x]
                                          (-> x
                                              (map-feature 6)
                                              (m/mmul best-theta)))))

#_(def contour-points (get-contour-points x-y-space 0 0.02))

#_(add-points data-plot (map first contour-points) (map second contour-points))

#_(view data-plot)


