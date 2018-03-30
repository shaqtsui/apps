(ns apps.ml-w5
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.tools.reader :as r]
            [taoensso.timbre :as timbre] ;; implicit require macro
            [clojure.core.matrix :as m]
            [clojure.core.matrix.stats :as m-s]
            [clojure.core.matrix.operators :as mo]
            [apps.dcs :refer [as dcs-or dcs-and parellel lazy-parellel]]
            [apps.image :as a-i]
            [apps.matrix :refer [del]]
            [mikera.image.core :as img]
            [clojure.math.numeric-tower :as math]
            [apps.ml-for-core-matrix :as a-ml]))

(m/set-current-implementation :vectorz)

(def X-v (-> "ml_w5/data.csv"
             io/resource
             io/reader
             line-seq
             (->> (map #(s/split % #","))
                  (m/emap r/read-string))))

;; convert impl
(def X (-> X-v
           m/matrix
           m/transpose))

(defn scalar->array [X]
  (-> X
      seq
      distinct
      count
      (as-> $
            (map #(-> (m/zero-array [$])
                      (m/mset %1 1))
                 X))
      m/matrix
      m/transpose))

(def Y-v (-> "ml_w5/datay.csv"
             io/resource
             io/file
             io/reader
             line-seq
             (->>
              (m/emap r/read-string))))

(def Y (-> Y-v
           (->> (map #(if (== % 10)
                        0
                        %))
                scalar->array)))

#_(def X-imgs (map #(a-i/seq->img % [20 20] (m/emin X) (m/emax X))
                   X))

#_(-> X-imgs
      (a-i/merge-imgs 100)
      img/show)

(def THETAs [(m/broadcast (m/scalar-array 1)
                          [25 401])
             (m/broadcast (m/scalar-array 1)
                          [10 26])])

(defn ACTs
  "X contains multiple vecotr of x in math formatter, perform x transform via base THETAs"
  [X THETAs]
  (reduce (fn [res THETA]
            (let [a-w-bias (m/join-along 0
                                         (m/broadcast (m/scalar-array 1)
                                                      [1 (m/column-count (last res))])
                                         (last res))]
              (conj (vec (butlast res)) ; vec to guarentee order when (butlast res) give nil
                    a-w-bias
                    (m/logistic (m/mmul THETA
                                        a-w-bias)))))
          [X]
          THETAs))

(defn nn-hypo-fn
  [THETAs]
  (fn [X]
    (-> X
        (ACTs THETAs)
        last)))

(defn nn-cost [THETAs X Y]
  (let [Y-hat ((nn-hypo-fn THETAs) X)]
    (* (- (/ 1
             (m/column-count X)))
       (m-s/sum (m-s/sum (mo/+ (mo/* Y
                                     (m/log Y-hat))
                               (mo/* (mo/- 1 Y)
                                     (m/log (mo/- 1 Y-hat)))))))))

#_(-> y
      (->> (map #(if (== % 10)
                   0
                   %))
           scalar->array
           (nn-cost ts X)))

(defn DELTA-last [ACT-last Y]
  (->
   (m/sub ACT-last Y)
   m/transpose
   m-s/sum
   (m/div (m/column-count ACT-last))))

(defn DELTA-i [DELTA-j ACT-i THETA-i]
  (mo/* (m/mmul (m/transpose THETA-i)
                DELTA-j)
        (->
         (mo/* ACT-i
               (mo/- 1 ACT-i))
         m/transpose
         m-s/sum)))

(defn DELTAs [ACTs Y THETAs]
  (reduce (fn [res v]
            (cons (DELTA-i (last res)
                           (first v)
                           (last v))
                  res))
          [(DELTA-last (last ACTs)
                       Y)]
          (map vector
               (->  ACTs
                    butlast
                    rest)
               (rest THETAs))))

(defn nn-gradient [X Y THETAs]
  (let [ans (-> X
                (ACTs THETAs))]

    (map (fn [an DELTA-j]
           (m/outer-product DELTA-j (-> an
                                          m/transpose
                                          m-s/sum)))
         (-> X
             (ACTs THETAs)
             butlast)
         (DELTAs ans Y THETAs))))

(-> nn-gradient
    (apply [X Y THETAs])
    last
    m/shape)
