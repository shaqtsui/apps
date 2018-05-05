(ns apps.ml-w7
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.tools.reader :as r]
            [taoensso.timbre :as timbre]
            [clojure.core.matrix :as m]
            [clojure.core.matrix.stats :as m-s]
            [clojure.core.matrix.operators :as mo]
            [clojure.data.csv :as csv]
            [apps.image :as a-i]
            [apps.matrix :as a-m]
            [mikera.image.core :as img]
            [clojure.math.numeric-tower :as math]
            [incanter.core :as i]
            [incanter.charts :as i-c]))

(m/set-current-implementation :vectorz)


(def X (-> "ml_w7/dataX.csv"
           io/resource
           io/reader
           csv/read-csv
           (->> (m/emap r/read-string))
           m/matrix))

(def Y (-> "ml_w7/dataY.csv"
           io/resource
           io/reader
           csv/read-csv
           (->> (m/emap r/read-string))
           m/matrix))


(i/view (i-c/scatter-plot (m/select X :all 0)
                          (m/select X :all 1)
                          :group-by Y))












