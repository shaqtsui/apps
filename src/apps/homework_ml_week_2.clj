(ns apps.homework-ml-week-2
  (:require [clojure.tools.logging :as log]
            [clojure.java.io :as io]
            [incanter.core :refer :all]
            [incanter.stats :refer :all]
            [incanter.charts :refer :all]
            [incanter.io :refer :all]
            [incanter.datasets :refer :all]
            [incanter.pdf :refer :all]))

;; machine learning by Andrew Ng
;; week 2

;; warm up exercise

(defn warmUpExercise []
  (identity-matrix 5))


;; plot data
(def ex1-data-1 (-> "homework_ml_week_2/ex1data1.txt"
    io/resource
    read-dataset
    to-matrix))
    
(defn plot-data [x y]
  (view (scatter-plot x y)))

;; gradient descent

(defn compute-cost [x y theta]
  (-> x
      (mmult theta)
      (minus y)
      (pow 2)
      ((partial apply +))
      (/ (count x))
      (/ 2)))
      
      
(defn dcost-dtheta [x y theta]
  (as-> x $
      (mmult $ theta)
      (minus $ y)
      (mmult (trans x) $)
      (div $ (count x))))

(defn gradient-descent [x y theta alpha num-iters]
  (if (= 0 num-iters)
    theta
    (recur x y
           (as-> theta $
                 (dcost-dtheta x y $)
                 (mult $ alpha)
                 (minus theta $))
           alpha
           (- num-iters 1))))

;; test it
(let [x (bind-columns (repeat (nrow ex1-data-1) 1) (sel ex1-data-1 :cols 0))
      y (sel ex1-data-1 :cols 1)
      init-theta [0 0]
      my-theta (gradient-descent x y [100 100] 0.01 5000)]
  (plot-data (sel ex1-data-1 :cols 0) (sel ex1-data-1 :cols 1))
  (warmUpExercise)
  (compute-cost x y init-theta)
  (dcost-dtheta x y init-theta)
  (gradient-descent x y [100 100] 0.01 5000)
  (doto (scatter-plot (sel x :cols 1) y)
    (add-function #(-> %
                       (* (-> my-theta rest first))
                       (+ (first my-theta))) 1 25)
    view)
  (doto (heat-map (fn [theta-0 theta-1]
                   (compute-cost x y [theta-0 theta-1]))
                 -4 -3 0.5 1.3)
   view))

      

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

;; test it
(let [x-to-be-norm (sel ex1-data-2 :cols [0 1])
      y (sel ex1-data-2 :cols 2)
      x (bind-columns (repeat (count ex1-data-2) 1) (feature-normalize x-to-be-norm))
      my-theta (gradient-descent x y [0 0 -5] 0.01 2000)]
  (feature-normalize x-to-be-norm)
  (as-> (range 0 500 10) $
      (map (partial gradient-descent x y [0 0 0] 0.01) $)
      (map (partial compute-cost x y) $)
      (scatter-plot (range 0 500 10) $)
      (view $))
  (as-> [1650 3] $
      (minus $ (fn-on-col mean x-to-be-norm))
      (div $ (fn-on-col sd x-to-be-norm))
      (trans $)
      (bind-columns [1] $)
      (mmult $ my-theta) ))
        
        

;; normal equation

(defn normal-equation [x y]
  (-> x
      trans
      (mmult x)
      solve
      (mmult (trans x))
      (mmult y)))


;; test it    
(let [x (as-> ex1-data-2 $
             (sel $ :cols [0 1])
             (bind-columns (repeat (count ex1-data-2) 1) $))
      y (sel ex1-data-2 :cols 2)]
  (mmult [[1 1650 3]] (normal-equation x y)))
