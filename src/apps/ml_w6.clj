(ns apps.ml-w6
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
           m/transpose))

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
           m/transpose))

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

(i/view (i-c/scatter-plot X Y))

(defn linear-reg-hypo-fn [THETA]
  (fn [X]
    (m/row-matrix (m/mmul THETA
                          (m/join-along 0
                                        (m/broadcast (m/scalar-array 1)
                                                     [1 (m/column-count X)])
                                        X)))))

(defn linear-reg-cost [THETA X Y lambda]
  (+ (* (/ 1
           (* 2
              (m/column-count X)))
        (m-s/sum (m-s/sum (m/pow (m/sub ((linear-reg-hypo-fn THETA) X)
                                        Y)
                                 2))))
     (* (/ lambda
           (* 2
              (m/column-count X)))
        (m-s/sum (m/pow (m/select THETA
                                  :rest)
                        2)))))

(linear-reg-cost [1 1] X Y 1)

(defn linear-reg-gradient [THETA X Y lambda]
  (m/add (mo/* (/ 1
                  (m/column-count X))
               ((comp m/transpose
                      m-s/sum
                      m/transpose)
                (m/mul (m/to-vector (m/sub ((linear-reg-hypo-fn THETA) X)   ; to-vector to support auto broadcast
                                           Y))
                       (m/join-along 0
                                     (m/broadcast (m/scalar-array 1)
                                                  [1 (m/column-count X)])
                                     X))))
         (mo/* (/ lambda
                  (m/column-count X))
               (m/join-along 0
                             (m/matrix [0])
                             (m/select THETA :rest)))))


(linear-reg-gradient [1 1] X Y 1)


