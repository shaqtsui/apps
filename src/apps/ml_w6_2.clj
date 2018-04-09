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

(a-m/linear-reg-gradient [1 1] X Y 1)

(def store (atom []))
(def monitor
  (fn [f X-0 opts]
    (if-let [iters (:iters opts)]
      (when (-> (mod iters 5)
                zero?)
        (let [y (f X-0)
              previous-y (last @store)]
          (timbre/debug "iters: " iters "\n" "y: " y)
          (if (or (nil? previous-y)
                  (< y previous-y))
            (reset! store (conj @store y))
            (throw (Exception. (str "NOT descending!!!\n" previous-y " -> " y))))))
      (reset! store []))))

(defn submatrixes-by-row-increase
  "[[1 2] [3 4]] -> ([[1 2]], [[1 2] [3 4]])"
  [X]
  (-> X
      m/row-count
      range
      (->> (map (comp (fn [cols]
                        (if (m/vec? X)
                          (m/select X cols)
                          (m/select X cols :all)))
                      range
                      inc)))))

(defn sublearnings [X Y Xval Yval lambda]
  (map (fn [X-sub Y-sub]
         [(a-m/fmin #(a-m/linear-reg-cost %
                                          X-sub
                                          Y-sub
                                          lambda)
                    (m/broadcast (m/scalar-array 1)
                                 [(inc (if (m/vec? X-sub)
                                         1
                                         (m/column-count X-sub)))])
                    :method :newton-raphson
                    :alpha 5E-2
                    :gradient-fn #(a-m/linear-reg-gradient %
                                                           X-sub
                                                           Y-sub
                                                           lambda)
                    :plugin monitor)
          X-sub
          Y-sub])
       (submatrixes-by-row-increase X)
       (submatrixes-by-row-increase Y)))

(defn learning-curve [sublearns Xval Yval]
  (let [costs-val (map (fn [[p _ _]]
                         (-> p
                             :X
                             (a-m/linear-reg-cost Xval Yval 0)))
                       sublearns)
        costs-train (map (fn [[p X-sub Y-sub]]
                           (-> p
                               :X
                               (a-m/linear-reg-cost X-sub Y-sub 0)))
                         sublearns)
        cost-c (i-c/xy-plot (range 1
                                   (inc (count costs-val)))
                            costs-val)]
    (i-c/add-lines cost-c
                   (range 1
                          (inc (count costs-train)))
                   costs-train)
    (i/view cost-c)))

(def sublearns (sublearnings X Y Xval Yval 0))
(clojure.pprint/pprint sublearns)
(def hy-fn (a-m/linear-reg-hypo-fn  (-> sublearns (nth 10) :X)))

(def dp
  (i-c/scatter-plot (m/select X [0 1 2]) (m/select Y [0 1 2])))
(i/view dp)

(learning-curve sublearns
                     Xval
                     Yval)

(def X-mf (a-m/map-feature X 8))
(def mu (m-s/mean X-mf))
(def sigma (m-s/sd X-mf))
(def X-poly (-> X-mf
                (a-m/feature-normalize :mean mu :sd sigma)))

(def sublearns (sublearnings X-poly
                           Y
                           (-> Xval
                               (a-m/map-feature 8)
                               (a-m/feature-normalize :mean mu :sd sigma))
                           Yval
                           0))

(clojure.pprint/pprint sublearns)

(learning-curve sublearns
                     (-> Xval
                         (a-m/map-feature 8)
                         (a-m/feature-normalize :mean mu :sd sigma))

                     Yval
)

(def dp
  (i-c/scatter-plot (m/select X [0 1 2 3 4 5 6 7 8 9 10 11]) (m/select Y [0 1 2 3 4 5 6 7 8 9 10 11])))

(def dp
  (i-c/scatter-plot Xval Yval))
(i/view dp)
(def hy-fn (a-m/linear-reg-hypo-fn  (-> curve (nth 11) :X)))

(i-c/add-function dp #(first (m/mul 1 (hy-fn (-> [%1]
                                                 (a-m/map-feature 8)
                                                 (a-m/feature-normalize :mean mu :sd sigma))))) -50 40)
