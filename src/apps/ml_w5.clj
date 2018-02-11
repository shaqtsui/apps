(ns apps.ml-w5
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.tools.reader :as r]
            [taoensso.timbre :as timbre] ;; implicit require macro
            [clojure.core.matrix :as m]
            [clojure.core.matrix.stats :as m-s]
            [apps.tools :refer [as]]
            [apps.image :as a-i]
            [mikera.image.core :as img]
            [clojure.math.numeric-tower :as math]
            [apps.ml-for-core-matrix :as a-ml]))

(def X (-> "/home/shark/xfcjs/Downloads/machine-learning-ex3/ex3/data.csv"
           io/file
           io/reader
           line-seq
           (->> (map #(s/split % #","))
                (m/emap r/read-string))))

#_(def X-imgs (map #(a-i/seq->img % [20 20] (m/emin X) (m/emax X))
                   X))

#_(-> X-imgs
      (a-i/merge-imgs 100)
      img/show)

(defn sigmoid
  "`X` is a vector"
  [X]
  (-> X m/sub m/exp
      (m/add 1)
      ((partial m/div 1))))

(defn hypo-fn
  "`theta` is a vector, return a function accecpt a matrix"
  [theta]
  (comp sigmoid #(m/mmul % theta)))

(defn get-cost [theta X Y & {:keys [lamb]}]
  (-> theta
      hypo-fn
      (apply [X])
      ((juxt #(-> %
                  m/log
                  (m/mul Y)
                  m/sub)
             #(-> %
                  m/sub
                  (m/add 1)
                  m/log
                  (m/mul (m/sub 1 Y)))))
      (->> (reduce m/sub))
      (cond->
       lamb (m/add (-> theta
                       (m/pow 2)
                       m/transpose
                       (m/mmul lamb))))))

