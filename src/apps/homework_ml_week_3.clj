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


;; start basic model functions (map matrix/vector)
(defn get-hypo-func [theta]
  (fn [x] 
    (-> (mmult x theta)
        sigmoid)))

(defn get-y-hat [hypo-func x]
  (hypo-func x))

(defn get-error [y-hat y]
  (minus y-hat y))

(defn get-cost [y-hat y]
    (-> y-hat log
        (mult y)
        minus
        (minus (-> y-hat
                   minus
                   (plus 1)
                   log
                   (mult (minus 1 y))))))

(defn get-derivetive-of-cost-on-theta 
  "theta change -> hypo func change -> cost change"
  [error x theta-index]
  (-> error
      (mult (sel x :cols theta-index))))


;; end basic model functions





(defn gradient-desent [x y start-theta alpha iter-num]
  (if (= 0 iter-num)
    start-theta
    (recur x y
           (as-> start-theta $
                 (get-hypo-func $)
                 (get-y-hat $ x)
                 (get-error $ y)
                 (partial get-derivetive-of-cost-on-theta $ x)
                 (map $ (range (ncol x)))
                 (map mean $)
                 (mult $ alpha)
                 (minus start-theta $))
           alpha
           (- iter-num 1))))

    
;;(def x-no-intercept (sel ex2-data-1 :cols [0 1]))
;;(def y (sel ex2-data-1 :cols 2))
;;(def x (add-intercept x-no-intercept))
;;
;;(def iters (range 0 1000000 100000))
;;(def optimized-thetas (map (partial gradient-desent x y [-4 0.04 0.03] 0.001) iters))
;;(def mean-costs (map (fn [theta]
;;                       (-> theta
;;                           get-hypo-func
;;                           (get-y-hat x)
;;                           (get-cost y)
;;                           mean))
;;                     optimized-thetas))
;;
;;(def cost-chart (line-chart iters mean-costs))
;;(view cost-chart)
;;
;;optimized-thetas
;;
;;(def optimized-theta (last optimized-thetas))
;;(to-list optimized-theta)
;;
;;(defn get-decision-boundary-func [theta]
;;  (fn [x-1] (-> (nth optimized-theta 1)
;;                (* x-1)
;;                -
;;                (- (nth optimized-theta 0))
;;                (/ (nth optimized-theta 2)))))
;;
;;(def data-plot (scatter-plot (sel x-no-intercept :cols 0) (sel x-no-intercept :cols 1) :group-by y))
;;(add-function data-plot (get-decision-boundary-func optimized-theta) 30 100)
;;(view data-plot)
;;
;;
;;(defn predict-new-data [theta x]
;;  (-> theta
;;      get-hypo-func
;;      (get-y-hat x)
;;      ((partial map #(if (>= % 0.5) 1 0)))))
;;
;;(defn accuracy [p y]
;;  (-> (map #(if (== %1 %2) 1 0) p y)
;;      sum
;;      (/ (count p))
;;      (* 100)))
;;
;;(sigmoid (mmult [[1 45 85]] optimized-theta))
;;(predict-new-data optimized-theta [[1 45 85]])
;;(accuracy (predict-new-data optimized-theta x) y)
;;

(def ex2-data-2
  (-> "homework_ml_week_3/ex2data2.txt"
      io/resource
      read-dataset
      to-matrix))

(def x-no-intercept (sel ex2-data-2 :cols [0 1]))
(def y (sel ex2-data-2 :cols 2))
(def x (add-intercept x-no-intercept))
(plot-data x-no-intercept y)



(defn get-poly-term [x degree]
  (let [x (matrix x)]
    (if (= 0 degree)
      (repeat (nrow x) 1)
      (if (= 1 (ncol x))
        (pow x degree)
        (as-> degree $
              (+ $ 1)
              (range $)
              (reverse $)
              (map (fn [d]
                     (as-> (get-poly-term (sel x :except-cols 0) (- degree d)) $$
                           (mult (apply bind-columns (repeat (ncol $$) (pow (sel x :cols 0) d))) $$)))
                   $)
              (apply bind-columns $))))))


(defn map-feature [x degree]
  (as-> (map (partial get-poly-term x) (range (+ degree 1))) $
        (apply bind-columns $)))


(map-feature [[2 3]] 2)
             
  
