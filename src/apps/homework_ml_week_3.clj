(ns apps.homework-ml-week-3
  (:require [clojure.tools.logging :as logging]
            [clojure.java.io :as io]
            [incanter.core :refer :all]
            [incanter.stats :refer :all]
            [incanter.charts :refer :all]
            [incanter.io :refer :all]
            [incanter.datasets :refer :all]
            [incanter.pdf :refer :all]
            [incanter.optimize :refer :all]))


(def ex2-data-1
  (-> "homework_ml_week_3/ex2data1.txt"
      io/resource
      read-dataset
      to-matrix))


(defn plot-data [x y]
  (-> (scatter-plot (sel x :cols 0) (sel x :cols 1) :group-by y)
      view))

;; map x
(defn add-intercept [x]
  (bind-columns (repeat
                  (count x)
                  1)
                x))

(defn init-theta [x]
  (repeat
    (ncol x)
    0))

;; tested ok
;; map z
(defn sigmoid [z]
  (-> z minus exp
      (plus 1)
      ((partial div 1))))


;; tested ok
;; return a func(map x) to map x(in vector) to y(in vector)
(defn create-hypo-func [theta]
  (fn [x] 
    (-> (mmult x theta)
        sigmoid)))

;; tested ok
;; bias between (hypo-func x) and real y
;; map x, y, hypo-func
(defn get-cost-vector [x y hypo-func]
  (let [y-estimate (logging/spy (hypo-func x))]
    (-> y-estimate log
        (mult y)
        minus
        (minus (-> y-estimate
                   ((partial minus 1))
                   log
                   (mult (minus 1 y)))))))


;; theta change -> hypo func change -> mean cost change
;; map theta
(defn dcost-dfunc-dtheta [x y theta]
  (-> theta
      create-hypo-func
      (apply [x])
      (minus y)
      ((partial mmult (trans x)))
      (div (count x))))



;; map theta
(defn gradient-desent [x y start-theta alpha iter-num]
  (if (= 0 iter-num)
    start-theta
    (recur x y
           (->> start-theta
                (dcost-dfunc-dtheta x y)
                (mult alpha)
                (minus start-theta))
           alpha
           (- iter-num 1))))
    
    
(def x-no-intercept (sel ex2-data-1 :cols [0 1]))
(def y (sel ex2-data-1 :cols 2))
(def x (add-intercept x-no-intercept))


(def plot (scatter-plot (sel x-no-intercept :cols 0) (sel x-no-intercept :cols 1) :group-by y))
(view plot)
(mean (get-cost-vector x y (create-hypo-func [0 0 0])))

(dcost-dfunc-dtheta x y [10 10 10])
(def optimized-theta (gradient-desent x y [10 10 10] 0.01 1000))


#_(as-> (range 0 100000 20000) $
  (map (partial gradient-desent x y [0 0 0] 0.005) $)
  (map to-list $))


(to-list optimized-theta)
(get-cost-vector x y (create-hypo-func optimized-theta))


(to-list optimized-theta)
(defn line [x] 
  (-> x
      (* (nth optimized-theta 1))
      -
      (- (nth optimized-theta 0))
      (/ (nth optimized-theta 2))))

(line 40)
(add-function plot line 0 100)
(view plot)
#_(minimize (fn [theta] (mean (get-cost-vector x y (create-hypo-func theta))))
          [0 0 0])

