(ns apps.homework-ml-week-3-newton
  (:require [clojure.tools.logging :as logging]
            [clojure.core.matrix :as m]
            [clojure.java.io :as io]
            [incanter.core :refer :all]
            [incanter.stats :refer :all]
            [incanter.charts :refer :all]
            [incanter.io :refer :all]
            [incanter.datasets :refer :all]
            [incanter.pdf :refer :all]
            [incanter.optimize :refer :all]
            [apps.ml :refer :all]))


(def ex2-data-1
  (-> "homework_ml_week_3/ex2data1.txt"
      io/resource
      read-dataset
      to-matrix))


(defn search-convergence-point [f del-f del-2-f X-0
                                & {:keys [max-iter method]
                                   :or {max-iter 200
                                        method :newton-raphson}
                                   :as opts}]
  (let [y (f X-0)]
    (if (or
          (= max-iter 0)
          (= y 0))
      {:X X-0 :y y}
      (recur f del-f del-2-f
             (- X-0
                (m/mmul (m/inverse (del-2-f X-0))
                        (del-f X-0)))
             (assoc opts :max-iter (dec max-iter))))))

(defn predict-new-data [theta x]
  (-> theta
      get-hypo-func
      (get-y-hat x)
      ((partial map #(if (>= % 0.5) 1 0)))))


(def x-no-intercept (sel ex2-data-1 :cols [0 1]))
(def y (sel ex2-data-1 :cols 2))
(def x (map-feature x-no-intercept 1))

(def lamb (-> (repeat (ncol x) 0.1)))

(def optimized-thetas (-> #(gradient-desent x y % lamb 0.002 500)
                          (iterate (init-theta x))
                          ((partial take 50))
                          doall
                          time))

(def mean-costs (map (fn [theta]
                       (-> (get-cost x y theta lamb)
                           mean))
                     optimized-thetas))
(def cost-chart (line-chart (range 50) mean-costs))
(view cost-chart)


(def optimized-theta (last optimized-thetas))
(to-list optimized-theta)

(defn get-decision-boundary-func [theta]
  (fn [x-1] (-> (nth optimized-theta 1)
                (* x-1)
                -
                (- (nth optimized-theta 0))
                (/ (nth optimized-theta 2)))))

(def data-plot (scatter-plot (sel x-no-intercept :cols 0) (sel x-no-intercept :cols 1) :group-by y))
(add-function data-plot (get-decision-boundary-func optimized-theta) 30 100)
(view data-plot)



(sigmoid (mmult [[1 45 85]] optimized-theta))
(predict-new-data optimized-theta [[1 45 85]])
(accuracy (predict-new-data optimized-theta x) y)






