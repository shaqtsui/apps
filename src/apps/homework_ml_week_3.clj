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



;; start util funcs for model
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



(defn init-theta [x]
  (repeat
    (ncol x)
    0))

(defn sigmoid [z]
  (-> z minus exp
      (plus 1)
      ((partial div 1))))

;; end util funcs for model

;; start basic model functions (map matrix/vector)
(defn get-hypo-func [theta]
  (fn [x]
    (-> (mmult x theta)
        sigmoid)))

(defn get-y-hat [hypo-func x]
  (hypo-func x))

(defn get-error [y-hat y]
  (minus y-hat y))

(defn get-cost [x y theta lamb]
  (let [y-hat (-> theta
                  get-hypo-func
                  (get-y-hat x))]
    (-> y-hat log
        (mult y)
        minus
        (minus (-> y-hat
                   minus
                   (plus 1)
                   log
                   (mult (minus 1 y))))
        (plus (-> theta
                  (pow 2)
                  trans
                  (mmult lamb)
                  first)))))

(defn get-derivetive-of-cost-on-theta
  "theta change -> hypo func change -> cost change"
  [x y theta lamb theta-index]
  (-> theta
      get-hypo-func
      (get-y-hat x)
      (get-error y)
      (mult (sel x :cols theta-index))
      (plus (* (nth lamb theta-index) 2 (nth theta theta-index)))))


;; end basic model functions



;; work on x, dx mean
(defn gradient-desent [x y start-theta lamb alpha iter-num]
  (if (= 0 iter-num)
    start-theta
    (recur x y
           (as-> (partial get-derivetive-of-cost-on-theta x y start-theta lamb) $
                 (map $ (range (count start-theta)))
                 (map mean $)
                 (mult $ alpha)
                 (minus start-theta $))
           lamb
           alpha
           (- iter-num 1))))


;; start make use of model funcs
(defn predict-new-data [theta x]
  (-> theta
      get-hypo-func
      (get-y-hat x)
      ((partial map #(if (>= % 0.5) 1 0)))))

(defn accuracy [p y]
  (-> (map #(if (== %1 %2) 1 0) p y)
      sum
      (/ (count p))
      (* 100)))

(defn get-x-y-space [x-1-s x-2-s f] 
  (as->  (for [x-1 x-1-s
               x-2 x-2-s]
           [x-1 x-2])
         $
         (bind-columns $ (f $))))


(defn get-contour-points [x-y-space target-y vari] 
  (as->  x-y-space $
         (filter (fn [point]
                   (-> point
                       last
                       (- target-y)
                       Math/abs
                       (< vari)))
                 $)))

;; end make use of model funcs



;;(def x-no-intercept (sel ex2-data-1 :cols [0 1]))
;;(def y (sel ex2-data-1 :cols 2))
;;(def x (map-feature x-no-intercept 1))
;;
;;(def lamb (-> (repeat (ncol x) 1)))
;;
;;(def optimized-thetas (-> #(gradient-desent x y % lamb 0.001 50000)
;;                          (iterate (init-theta x))
;;                          ((partial take 16))
;;                          doall
;;                          time))
;;
;;(def mean-costs (map (fn [theta]
;;                       (-> (get-cost x y theta lamb)
;;                           mean))
;;                     optimized-thetas))
;;(def cost-chart (line-chart iters mean-costs))
;;(view cost-chart)
;;
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
;;
;;(sigmoid (mmult [[1 45 85]] optimized-theta))
;;(predict-new-data optimized-theta [[1 45 85]])
;;(accuracy (predict-new-data optimized-theta x) y)






;; regulation 
(def ex2-data-2
  (-> "homework_ml_week_3/ex2data2.txt"
      io/resource
      read-dataset
      to-matrix))

(def x-no-intercept (sel ex2-data-2 :cols [0 1]))
(def y (sel ex2-data-2 :cols 2))
(def x (map-feature x-no-intercept 6))

(def data-plot (scatter-plot (sel x-no-intercept :cols 0) (sel x-no-intercept :cols 1) :group-by y))
(view data-plot)

(def lamb (-> (repeat (- (ncol x) 1) 0)
              (conj 0)
              (div (nrow x))
              (div 2)))

lamb


#_(def start-theta (init-theta x))
(def start-theta [2.187322090205406 1.4526185676360428 2.3810044165424435 -3.601591772007569 -2.1826441122875484 -2.6615722285596073 0.4629887130368051 -0.8583602872167071 -0.7799742298462007 -0.23459935505741322 -2.7649840606422944 -0.14394622231159743 -1.2488738856929584 -0.7200542808026559 -2.2754365237628034 -0.3829384371982605 -0.47447146999775897 -0.05170482271037539 -0.6152572147583071 -0.7158688435289536 -0.7541403120837197 -2.043925566672119 0.04218672999571941 -0.5932391097186545 0.022405904297492344 -0.6849129292620665 -0.4208699499266736 -1.722317817932494])


(def optimized-thetas (-> #(gradient-desent x y % lamb 10 500)
                          (iterate start-theta)
                          ((partial take 20))
                          doall
                          time))

(def mean-costs (map (fn [theta]
                       (-> (get-cost x y theta lamb)
                           mean))
                     optimized-thetas))
(def cost-chart (line-chart (range 20) mean-costs))
(view cost-chart)

(def optimized-theta (last optimized-thetas))
(to-list optimized-theta)

(-> (get-cost x y (init-theta x) lamb)
    mean)
(-> (get-cost x y optimized-theta lamb)
    mean)
(accuracy (predict-new-data optimized-theta x) y)



(def u (-> (range -1 1.5 (/ 2.5 500))))
(def v u)

(def x-y-space (get-x-y-space u v (fn [x]
                                          (-> x
                                              (map-feature 6)
                                              (mmult optimized-theta)))))
(def contour-points (get-contour-points x-y-space 0 0.02))

(add-points data-plot (map first contour-points) (map second contour-points))


