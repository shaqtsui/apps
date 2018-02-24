(ns apps.ml-w5
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.tools.reader :as r]
            [taoensso.timbre :as timbre] ;; implicit require macro
            [clojure.core.matrix :as m]
            [clojure.core.matrix.stats :as m-s]
            [apps.dcs :refer [as dcs-or dcs-and parellel lazy-parellel]]
            [apps.image :as a-i]
            [apps.matrix :refer [del]]
            [mikera.image.core :as img]
            [clojure.math.numeric-tower :as math]
            [apps.ml-for-core-matrix :as a-ml]))

(def X (-> "ml_w5/data.csv"
           io/resource
           io/reader
           line-seq
           (->> (map #(s/split % #","))
                (m/emap r/read-string))))

(def y (-> "ml_w5/datay.csv"
           io/resource
           io/file
           io/reader
           line-seq
           (->>
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

(defn lr-hypo-fn
  "`theta` is a vector, return a function accecpt a matrix"
  [theta]
  (comp sigmoid #(m/mmul % theta)))

(defn lr-cost [theta X Y & {:keys [lamb]}]
  (-> theta
      lr-hypo-fn
      (apply [X])
      (parellel (-> m/log
                    (m/mul Y)
                    m/sub)
                (-> m/sub
                    (m/add 1)
                    m/log
                    (m/mul (m/sub 1 Y))))
      (->> (reduce m/sub))
      m-s/mean
      (cond->
       lamb (+ (-> theta
                   (m/pow 2)
                   m-s/mean
                   (/ 2)
                   (* lamb))))))

(defn lr-gradient [theta X Y & {:keys [lamb]}]
  (-> theta
      lr-hypo-fn
      (apply [X])
      (m/sub Y)
      (->> (m/mmul (m/transpose X)))
      (m/div (count X))
      (cond->
       lamb (m/add (-> theta
                       (m/mul (/ lamb (count X)))
                       rest
                       (->> (cons 0)))))))

(defn search-convergence-point [f X-0
                                & {:keys [gradient-fn hessian-fn max-iter method]
                                   :or {max-iter 200
                                        method :newton-raphson
                                        gradient-fn (del f)
                                        hessian-fn (del gradient-fn)}
                                   :as opts}]

  (-> X-0
      (lazy-parellel f (-> hessian-fn
                           m/inverse))
      timbre/spy
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

;;(search-convergence-point #(lr-cost % X y) (repeat 400 1) :gradient-fn #(lr-gradient % X y))


(def ts [(m/broadcast 1 [25 401]) (m/broadcast 1 [10 26])])

(defn nn-hypo-fn
  [ts]
  (fn [X]
    (-> X
        (as $ (reduce #(-> %1
                           (->> (m/join-along 1 (m/broadcast 1 [(m/row-count %1) 1])))
                           (m/mmul (-> %2 m/transpose))
                           sigmoid)
                      $
                      ts)))))

