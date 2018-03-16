(ns apps.ml-w5-2
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

(def y (-> "ml_w5/datay.csv"
           io/resource
           io/file
           io/reader
           line-seq
           (->>
            (m/emap r/read-string))
           m/matrix))

#_(def X-imgs (map #(a-i/seq->img % [20 20] (m/emin X) (m/emax X))
                   (m/columns X)))

#_(-> X-imgs
      (a-i/merge-imgs 100)
      img/show)

(defn del-logistic [& {:keys [X Y]
                       :or {Y (m/logistic X)}}]
  (m/mul Y
         (m/sub 1
                Y)))

(defn search-convergence-point [f X-0
                                & {:keys [gradient-fn hessian-fn max-iter method]
                                   :or {max-iter 200
                                        method :newton-raphson
                                        gradient-fn (del f)
                                        hessian-fn (del gradient-fn)}
                                   :as opts}]

  (-> X-0
      (lazy-parellel f (-> timbre/spy
                           hessian-fn
                           timbre/spy
                           m/inverse
                           timbre/spy
                           time))
      (dcs-or (-> first
                  (dcs-and (-> (= 0)
                               (or (= max-iter 0)))
                           (as $ {:X X-0 :y $ :iter (- 200 max-iter)})))
              (dcs-or
               (dcs-and (-> second  nil?)
                        (as $ {:X X-0 :y (first $) :iter (- 200 max-iter) :error (format "Inverse of hessian is nil. hessian is: %s" (-> $ second))}))
               (-> second
                   (as $ (recur f
                                (m/sub X-0
                                       (m/mmul $
                                               (gradient-fn X-0)))
                                (assoc opts :max-iter (dec max-iter)))))))))

#_(search-convergence-point #(lr-cost % X (m/eq y 1)) (->> (repeatedly rand)
                                                           (take 400)) :gradient-fn #(lr-gradient % X (m/eq y 1)))
#_(search-convergence-point #(lr-cost % X y) (->> (repeatedly rand)
                                                  (take 400)))

(def ts [(m/broadcast (m/scalar-array 1)
                      [25 401])
         (m/broadcast (m/scalar-array 1)
                      [10 26])])

(defn nn-hypo-fn
  "X contains multiple vecotr of x in math formatter, perform x transform via base ts"
  [ts]
  (fn [X]
    (reduce #(m/logistic (m/mmul %2
                                 (m/join-along 0
                                               (m/broadcast (m/scalar-array 1)
                                                            [1 (m/column-count %1)])
                                               %1)))
            X
            ts)))

(defn nn-cost [ts X Y]
  (let [y-hat ((nn-hypo-fn ts) X)]
    (* (- (/ 1
             (m/column-count X)))
       (m-s/sum (mo/+ (mo/* Y
                            (m/log y-hat))
                      (mo/* (mo/- 1 Y)
                            (m/log (mo/- 1 y-hat))))))))

(defn scalar->array [X]
  (->
   (map #(-> (m/zero-array [%2])
             (m/mset %1 1))
        X
        (-> X
            seq
            distinct
            count
            repeat))
   m/matrix
   m/transpose))

(defn dcost-over-dzlast [a y]
  (m/sub a y))

(defn dcost-over-dzn [dcost-over-dzn+1 an thetan]
  (mo/* (m/mmul (m/transpose thetan)
                dcost-over-dzn+1)
        (mo/* an
              (mo/- 1 an))))

#_(-> y
      (->> (map #(if (== % 10)
                   0
                   %))
           scalar->array
           (nn-cost ts X)))

