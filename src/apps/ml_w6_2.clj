(ns apps.ml-w6-2
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.tools.reader :as r]
            [taoensso.timbre :as timbre]
            [clojure.core.matrix :as m]
            [clojure.core.matrix.stats :as m-s]
            [clojure.core.matrix.operators :as mo]
            [apps.image :as a-i]
            [apps.matrix :as a-m]
            [mikera.image.core :as img]
            [clojure.math.numeric-tower :as math]
            [incanter.core :as i]
            [incanter.charts :as i-c]))

(m/set-current-implementation :vectorz)

(def X (-> "ml_w6/dataX.csv"
           io/resource
           io/reader
           line-seq
           (->> (map #(s/split % #","))
                (m/emap r/read-string))
           m/matrix
           m/to-vector))

(def Xtest (-> "ml_w6/dataXtest.csv"
               io/resource
               io/reader
               line-seq
               (->> (map #(s/split % #","))
                    (m/emap r/read-string))
               m/matrix
               m/to-vector))

(def Xval (-> "ml_w6/dataXval.csv"
              io/resource
              io/reader
              line-seq
              (->> (map #(s/split % #","))
                   (m/emap r/read-string))
              m/matrix
              m/to-vector))

(def Y (-> "ml_w6/dataY.csv"
           io/resource
           io/reader
           line-seq
           (->> (map #(s/split % #","))
                (m/emap r/read-string))
           m/matrix
           m/to-vector))

(def Ytest (-> "ml_w6/dataYtest.csv"
               io/resource
               io/reader
               line-seq
               (->> (map #(s/split % #","))
                    (m/emap r/read-string))
               m/matrix
               m/to-vector))

(def Yval (-> "ml_w6/dataYval.csv"
              io/resource
              io/reader
              line-seq
              (->> (map #(s/split % #","))
                   (m/emap r/read-string))
              m/matrix
              m/to-vector))

(def dp
  (i-c/scatter-plot X Y))
(i/view dp)

(defn linear-reg-hypo-fn [THETA]
  (fn [X]
    (m/mmul (m/join-along 1
                          (m/broadcast (m/scalar-array 1)
                                       [(m/row-count X) 1])
                          (m/column-matrix X))
            THETA)))

(defn linear-reg-cost [THETA X Y lambda]
  (+ (* (/ 1
           (* 2
              (m/row-count X)))
        (m-s/sum (m/pow (m/sub ((linear-reg-hypo-fn THETA) X)
                               Y)
                        2)))
     (* (/ lambda
           (* 2
              (m/row-count X)))
        (m-s/sum (m/pow (m/select THETA
                                  :rest)
                        2)))))

(linear-reg-cost [1 1] X Y 1)

(defn linear-reg-gradient [THETA X Y lambda]
  (m/add (mo/* (/ 1
                  (m/row-count X))
               (m-s/sum (m/mul (m/transpose (m/broadcast (m/sub ((linear-reg-hypo-fn THETA) X)
                                                                Y)
                                                         [2 (m/row-count Y)]))
                               (m/join-along 1
                                             (m/broadcast (m/scalar-array 1)
                                                          [(m/row-count X) 1])
                                             (m/column-matrix X)))))
         (mo/* (/ lambda
                  (m/row-count X))
               (m/join-along 0
                             (m/matrix [0])
                             (m/select THETA :rest)))))

(linear-reg-gradient [1 1] X Y 1)

(def store (atom []))
(def monitor
  (fn [f X-0 opts]
    (if-let [iters (:iters opts)]
      (when (-> (mod iters 100)
                zero?)
        (let [y (f X-0)
              previous-y (last @store)]
          (timbre/debug "iters: " iters "\n" "y: " y)
          (if (or (nil? previous-y)
                  (< y previous-y))
            (reset! store (conj @store y))
            (throw (Exception. (str "NOT descending!!!\n" previous-y " -> " y))))))
      (reset! store []))))

(def p (a-m/fmin-precision #(linear-reg-cost %
                                             X
                                             Y
                                             0)
                           [1 1]
;;                           :method :newton-raphson
                           :method :gradient-desent
                           :alpha 2E-3

                           :gradient-fn #(linear-reg-gradient %
                                                              X
                                                              Y
                                                              0)
                           :plugin monitor))

(linear-reg-gradient (:X p) X Y 0)

#_(i/view (i-c/scatter-plot (range (count @store)) @store))

(defn accumulate [xs]
  (reduce (fn [res x]
            (conj res
                  (if-let [x-p (last res)]
                    (conj x-p x)
                    [x])))
          []
          xs))

(defn learning-curve [X Y lambda]
  (map (fn [X-accu Y-accu]
         (a-m/fmin-precision #(linear-reg-cost %
                                               X-accu
                                               Y-accu
                                               lambda)
                             [1 1]
                             :method ;;:newton-raphson
                             :gradient-desent
                             :precision 1E-2
                             :alpha 5E-4
                             :gradient-fn #(linear-reg-gradient %
                                                                X-accu
                                                                Y-accu
                                                                lambda)
                             :plugin monitor))

       (reverse (accumulate X))
       (reverse (accumulate Y))))

(def curve (learning-curve X Y 0))

(def costs-val
  (map (fn [p]
         (-> p
             :X
             (linear-reg-cost Xval Yval 0)))
       (reverse curve)))

(def costs-train
  (map (fn [p X-accu Y-accu]
         (-> p
             :X
             (linear-reg-cost X-accu Y-accu 0)))
       (reverse curve)
       (accumulate X)
       (accumulate Y)))

(def cost-c
  (i-c/xy-plot (range 1
                      (inc (count costs-val)))
               costs-val))

(i-c/add-lines cost-c
               (range 1
                      (inc (count costs-train)))
               costs-train)

(i/view cost-c)

 (a-m/poly-term [1 2] 3)

(-> X
    (a-m/map-feature 8)
    a-m/feature-normalize
    a-m/add-constant-comp
   )

