(ns apps.ml
  (:require [clojure.tools.logging :as logging]
            [clojure.java.io :as io]
            [incanter.core :refer :all]
            [incanter.stats :refer :all]
            [incanter.charts :refer :all]
            [incanter.io :refer :all]
            [incanter.datasets :refer :all]
            [incanter.pdf :refer :all]
            [incanter.optimize :refer :all]
            [incanter.latex :refer :all]))

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
(defn to-y-class [y c]
  (map #(if (= % c)
          1
          0)
       y))


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
    (-> y-hat
        log
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

