(ns apps.ml-w9
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.tools.reader :as r]
            [taoensso.timbre :as timbre]
            [clojure.core.matrix :as m]
            [clojure.core.matrix.stats :as m-s]
            [clojure.core.matrix.linear :as m-l]
            [clojure.data.csv :as csv]
            [apps.image :as a-i]
            [apps.matrix :as a-m]
            [mikera.image.core :as img]
            [mikera.image-matrix :as img-m]
            [clojure.math.numeric-tower :as math]
            [incanter.core :as i]
            [incanter.charts :as i-c]
            [taoensso.tufte :as tufte :refer [defnp fnp]]))

(m/set-current-implementation :vectorz)


(def X (-> "ml_w9/dataX.csv"
           io/resource
           io/reader
           csv/read-csv
           (->> (m/emap r/read-string))
           m/matrix))

(def Xval (-> "ml_w9/dataXval.csv"
              io/resource
              io/reader
              csv/read-csv
              (->> (m/emap r/read-string))
              m/matrix))

(def Yval (-> "ml_w9/dataYval.csv"
              io/resource
              io/reader
              csv/read-csv
              (->> (m/emap r/read-string))
              m/matrix
              m/to-vector))


(def plot (a-m/scatter-plot X))


(def gd (a-m/estimate-gaussian X))




(m-s/sum (m/mul (a-m/single-var-gaussian (m/select X
                                                   :all
                                                   :first)
                                         (m/select (:mu gd)
                                                   :first)
                                         (m/select (:sigma2 gd)
                                                   :first))
                (a-m/single-var-gaussian (m/select X
                                                   :all
                                                   :last)
                                         (m/select (:mu gd)
                                                   :last)
                                         (m/select (:sigma2 gd)
                                                   :last))))

(m-s/sum (a-m/mul-var-gaussian X (:mu gd) (m/diagonal-matrix (:sigma2 gd))))


(def pval (a-m/mul-var-gaussian Xval (:mu gd) (m/diagonal-matrix (:sigma2 gd))))

(def best-res (a-m/select-threshold pval Yval))

(def p (a-m/mul-var-gaussian X (:mu gd) (m/diagonal-matrix (:sigma2 gd))))


(a-m/add-points plot (a-m/anomalies X p (:epsilon best-res)))




(def X2 (-> "ml_w9/dataX2.csv"
            io/resource
            io/reader
            csv/read-csv
            (->> (m/emap r/read-string))
            m/matrix))

(def X2val (-> "ml_w9/dataX2val.csv"
               io/resource
               io/reader
               csv/read-csv
               (->> (m/emap r/read-string))
               m/matrix))

(def Y2val (-> "ml_w9/dataY2val.csv"
               io/resource
               io/reader
               csv/read-csv
               (->> (m/emap r/read-string))
               m/matrix
               m/to-vector))

(def gdist (a-m/estimate-gaussian X2))

(a-m/mul-var-gaussian X2 (:mu gdist) (m/diagonal-matrix (:sigma2 gdist)))


(def best-res (a-m/select-threshold (a-m/mul-var-gaussian X2val (:mu gdist) (m/diagonal-matrix (:sigma2 gdist)))
                                    Y2val))


best-res

(count (seq (a-m/anomalies X2
                           (a-m/mul-var-gaussian X2 (:mu gdist) (m/diagonal-matrix (:sigma2 gdist)))
                           (:epsilon best-res))))




(def Ymov (-> "ml_w9/dataYmov.csv"
              io/resource
              io/reader
              csv/read-csv
              (->> (m/emap r/read-string))
              m/matrix))

(def Rmov (-> "ml_w9/dataRmov.csv"
              io/resource
              io/reader
              csv/read-csv
              (->> (m/emap r/read-string))
              m/matrix))

(def Xmov (-> "ml_w9/dataXmov.csv"
              io/resource
              io/reader
              csv/read-csv
              (->> (m/emap r/read-string))
              m/matrix))

(def Thetamov (-> "ml_w9/dataThetamov.csv"
                  io/resource
                  io/reader
                  csv/read-csv
                  (->> (m/emap r/read-string))
                  m/matrix))

(def NUmov 943)

(def NMmov 1682)

(def NFmov 10)


(def NUmov 4)

(def NMmov 5)

(def NFmov 3)



(m/shape Ymov)
(m/shape Rmov)
(m/shape Xmov)
(m/shape Thetamov)

NUmov
NMmov
NFmov

(m-s/mean (m/select Ymov
                    :first
                    (first (m/non-zero-indices Rmov))))


(defn cofi-cost
  "`R` - 1, 0 matrix indicate where rated corresponding to Y"
  [THETA X R Y lambda]
  (+ (* (/ 1 2)
        (m/esum (m/pow (m/sub (m/mul (m/mmul X
                                             (m/transpose THETA))
                                     R)
                              Y)
                       2)))
     (* (/ lambda 2)
        (m/esum (m/pow THETA
                       2)))))

(defn cofi-gradient
  [THETA X R Y lambda]
  (+ (* (/ 1 2)
        (m/ (m/sub (m/mul (m/mmul X
                                      (m/transpose THETA))
                              R)
                       Y)))
     (* lambda THETA)))


(def X Xmov)
(def Y Ymov)

(m/broadcast X
             (cons (m/column-count Y)
                     (m/shape X)))

(cofi-cost Thetamov Xmov Rmov Ymov 0)


(cofi-cost (m/select Thetamov
                (range NUmov)
                (range NFmov))
      (m/select Xmov
                (range NMmov)
                (range NFmov))
      (m/select Rmov
                (range NMmov)
                (range NUmov))
      (m/select Ymov
                (range NMmov)
                (range NUmov))
      0)


