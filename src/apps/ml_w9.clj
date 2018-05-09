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

(def X Xmov)
#_(def Y Ymov)
#_(def R Rmov)

(def my-rates (-> (m/zero-vector (m/row-count Ymov))
                  (m/set-indices [1 98 7 12 54 64 66 69 183 226 355] [4 2 3 5 4 5 3 5 4 5 5])))

(def Y (m/join-along 1
                     (m/column-matrix my-rates)
                     Ymov))
(def R (m/join-along 1
                     (m/column-matrix (m/ne my-rates
                                            (m/zero-vector (m/row-count Ymov))))
                     Rmov))

(def THETA Thetamov)
(def lambda 10)

;; check gradient
(a-m/cofi-cost THETA X R Y lambda)
(a-m/cofi-gradient-x THETA X R Y lambda)
(a-m/cofi-gradient-theta THETA X R Y lambda)

((a-m/del2 #(a-m/cofi-cost THETA % R Y lambda)) X)
((a-m/del2 #(a-m/cofi-cost % X R Y lambda)) THETA)


;; learning
(def rate-mean-per-movie (m/div (m-s/sum (m/transpose Y))
                                (m-s/sum (m/transpose R))))

(def Y-norm (m/mul (m/sub Y
                          (a-m/broadcast-on rate-mean-per-movie 1 [(m/column-count Y)]))
                   R))

Y-norm

(def temp (a-m/vector->matrixs (distinct (repeatedly rand))
                               [(m/column-count Y-norm) 10] [(m/row-count Y-norm) 10]))
(def init-THETA (first temp))
(def init-X (second temp))

(def store (atom nil))

(tufte/add-basic-println-handler! {})

(tufte/profile
 {}
 (a-m/fmin #(a-m/cofi-cost  %
                           (m/shape init-THETA)
                           (m/shape init-X)
                           R
                           Y-norm
                           lambda)
          (a-m/matrixs->vector init-THETA init-X)
          :gradient-fn #(a-m/cofi-gradient %
                                           (m/shape init-THETA)
                                           (m/shape init-X)
                                           R Y-norm lambda)
          :alpha 2E-3
          :plugin (juxt (a-m/fmin-plugin-plot 3)
                        (a-m/fmin-plugin-last-x 1 store))))

(def THETA-X @store)
#_(csv/write-csv (io/writer "last-THETA-X.csv")
                 THETA-X)
#_(def temp (a-m/vector->matrixs THETA-X
                               [(m/column-count Y-norm) 10] [(m/row-count Y-norm) 10]))

(def tx (a-m/vector->matrixs THETA-X (m/shape init-THETA) (m/shape init-X)))

(def best-THETA (first tx))
(def best-X (last tx))

(m/esum best-THETA)
(m/esum best-X)

(def p (m/mmul best-X
               (m/transpose best-THETA)))

(m/esum (m/mul R
               (m/sub p
                      Y-norm)))

(def my-rates (m/add (m/get-column p 0)
                     rate-mean-per-movie))

(def my-selected-rates (m/select-indices my-rates [1 98 7 12 54 64 66 69 183 226 355]))
my-selected-rates
;; [4 2 3 5 4 5 3 5 4 5 5]
(reverse (sort-by last
                  (m/emap-indexed vector my-rates)))


