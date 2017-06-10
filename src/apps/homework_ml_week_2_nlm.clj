(ns apps.homework-ml-week-2-nlm
  (:require [clojure.tools.logging :as logging]
            [clojure.java.io :as io]
            [incanter.core :refer :all]
            [incanter.stats :refer :all]
            [incanter.charts :refer :all]
            [incanter.io :refer :all]
            [incanter.datasets :refer :all]
            [incanter.pdf :refer :all]
            [incanter.optimize :refer :all]))



;; plot data
(def ex1-data-1 (-> "homework_ml_week_2/ex1data1.txt"
                    io/resource
                    read-dataset
                    to-matrix))

(defn plot-data [x y]
  (view (scatter-plot x y)))



;; nlm
(let [x (bind-columns (repeat (nrow ex1-data-1) 1) (sel ex1-data-1 :cols 0))
      y (sel ex1-data-1 :cols 1)]
  (-> (non-linear-model #(first (mmult %2 %1)) y x [1 1])))


;; multi variable


(def ex1-data-2 (-> "homework_ml_week_2/ex1data2.txt"
                    io/resource
                    read-dataset
                    to-matrix))


;; feature normalize
(defn fn-on-col [f m]
  (->> m
       trans
       (map f)))

(defn feature-normalize [x]
  (-> x
      (minus (repeat (count x) (fn-on-col mean x)))
      (div (repeat (count x) (fn-on-col sd x)))))


#_(let [x-to-be-norm (sel ex1-data-2 :cols [0 1])
      y (sel ex1-data-2 :cols 2)
      x (bind-columns (repeat (count ex1-data-2) 1) (feature-normalize x-to-be-norm))]
  (-> (non-linear-model #(first (mmult %2 %1)) y x [1 1 1])
      :coefs)
  (as-> [1650 3] $
        (minus $ (fn-on-col mean x-to-be-norm))
        (div $ (fn-on-col sd x-to-be-norm))
        (trans $)
        (bind-columns [1] $)
        (mmult $ (-> (non-linear-model #(first (mmult %2 %1)) y x [1 1 1]) :coefs))
        (to-list $)))

