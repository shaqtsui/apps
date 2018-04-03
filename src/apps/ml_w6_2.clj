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

(i/view (i-c/scatter-plot X Y))

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
    (when (-> opts
              :max-iter
              ((fnil mod 0) 2)
              zero?)
      (let [y (f X-0)
            previous-y (last @store)]
        (timbre/debug "max-iter: " (:max-iter opts) "\n" "y: " y)
        (if (or (nil? previous-y)
                (< y previous-y))
          (reset! store (conj @store y))
          (do
            (reset! store [])
            (throw (Exception. (str "NOT descending!!!\n" previous-y " -> " y)))))))))

(a-m/fmin #(linear-reg-cost %
                        X
                        Y
                        0)
      [1 1]
      :gradient-fn #(linear-reg-gradient %
                                         X
                                         Y
                                         0)
      :alpha 2E-3
      :plugin monitor)

(i/view (i-c/scatter-plot (range (count @store)) @store))
