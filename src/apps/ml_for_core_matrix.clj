(ns apps.ml-for-core-matrix
  (:require [clojure.tools.logging :as logging]
            [clojure.core.matrix :as m]
            [clojure.core.matrix.stats :as m-s]
            [clojure.java.io :as io]))

(m/set-current-implementation :vectorz)

;; start util funcs for model
(defn get-poly-term
  "E.g. (get-poly-term [[x y z]] 2) -> [[x^2 xy xz y^2 yz z^2]]"
  [x degree]
  (if (= 0 degree)
    (repeat (m/row-count x) [1])
    (if (= 1 (m/column-count x))
      (m/pow x degree)
      (as-> degree $
            (+ $ 1)
            (range $)
            (reverse $)
            (map (fn [d]
                   (as-> (get-poly-term
                           (m/select x :all (range 1 (m/column-count x)))
                           (- degree d))
                         $$
                         (m/mul
                           (m/transpose (repeat
                                          (m/column-count $$)
                                          (m/pow (m/select x :all 0) d)))
                           $$)))
                 $)
            (apply m/join-along 1 $)))))



(defn map-feature [x degree]
  (as-> (map (partial get-poly-term x) (range (+ degree 1))) $
        (apply m/join-along 1 $)))


(defn init-theta [x]
  (repeat
    (m/column-count x)
    0))


(defn sigmoid [z]
  (-> z m/sub m/exp
      (m/add 1)
      ((partial m/div 1))))


;; end util funcs for model



;; start basic model functions (map matrix/vector)
(defn to-y-class [y c]
  (map #(if (= % c)
          1
          0)
       y))


(defn get-hypo-func [theta]
  (fn [x]
    (-> (m/mmul x theta)
        sigmoid)))

(defn get-y-hat [hypo-func x]
  (hypo-func x))

(defn get-error [y-hat y]
  (m/sub y-hat y))

(defn get-cost [x y theta lamb]
  (let [y-hat (-> theta
                  get-hypo-func
                  (get-y-hat x))]
    (-> y-hat
        m/log
        (m/mul y)
        m/sub
        (m/sub (-> y-hat
                   m/sub
                   (m/add 1)
                   m/log
                   (m/mul (m/sub 1 y))))
        (m/add (-> theta
                  (m/pow 2)
                  m/transpose
                  (m/mmul lamb))))))

(defn get-derivetive-of-cost-on-theta
  "theta change -> hypo func change -> cost change"
  [x y theta lamb theta-index]
  (-> theta
      get-hypo-func
      (get-y-hat x)
      (get-error y)
      (m/mul (m/select x :all theta-index))
      (m/add (* (nth lamb theta-index) 2 (nth theta theta-index)))))


;; end basic model functions


;; work on x, dx mean
(defn gradient-desent [x y start-theta lamb alpha iter-num]
  (if (= 0 iter-num)
    start-theta
    (recur x y
           (as-> (partial get-derivetive-of-cost-on-theta x y start-theta lamb) $
                 (map $ (range (count start-theta)))
                 (map m-s/mean $)
                 (m/mul $ alpha)
                 (m/sub start-theta $))
           lamb
           alpha
           (- iter-num 1))))


;; start make use of model funcs

(defn accuracy [p y]
  (-> (map #(if (== %1 %2) 1 0) p y)
      m-s/sum
      (/ (count p))
      (* 100)))

(defn get-x-y-space [x-1-s x-2-s f] 
  (as->  (for [x-1 x-1-s
               x-2 x-2-s]
           [x-1 x-2])
         $
         (m/join-along 1 $ (f $))))


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


