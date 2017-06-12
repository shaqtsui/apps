(ns apps.homework-ml-week-2-newton
  (:require [clojure.tools.logging :as logging]
            [clojure.java.io :as io]
            [clojure.core.matrix :as m]
            [clojure.core.matrix.stats :as m-s]
            [clojure.tools.reader :as r]))

;; this the only tested ok example in all week's homework
(defn del
  "
  impl del in math via samplar.
  E.g. 1
    f: [x y z] -> v
    (del f): [x y z] -> [v1 v2 v3]

  E.g. 2
    f: [x1 x2 x3 ... xm] -> [y1 y2 y3 ... yn]
    (del f): [x1 x2 x3 ... xm] -> (n colum, m row matrix)
  "
  [f & {:keys [dx]
        :or {dx 1E-4}}]
  (fn [X]
    (-> dx
        (m/broadcast [(count X)])
        m/diagonal-matrix
        (m/add X)
        ((partial map f))
        (m/sub (f X))
        (m/div dx))))



(defn search-convergence-point [f X-0
                                & {:keys [del-f del-2-f max-iter method]
                                   :or {max-iter 200
                                        method :newton-raphson
                                        del-f (del f :dx 0.1)
                                        del-2-f (del del-f :dx 0.1)}
                                   :as opts}]
  (let [y (f X-0)]
    (if (or
         (= max-iter 0)
         (= y 0))
      {:X X-0 :y y :iter (- 200 max-iter)}
      (let [del-2 (del-2-f X-0)
            del-2-inverse (m/inverse del-2)]
        (if (= nil del-2-inverse)
          {:X X-0 :y y :iter (- 200 max-iter) :error (format "Inverse of del-2 is nil. del-2: %s" (del-2-f X-0))}
          (recur f
                 (m/sub X-0

                        (m/mmul del-2-inverse
                                (del-f X-0)))
                 (assoc opts :max-iter (dec max-iter))))))))

(defn del-f-generator [x y]
  #(-> (m/mmul x %)
       (m/sub y)
       (m/mul 2)
       (m/broadcast [(count %) (m/row-count x)])
       m/transpose
       (m/mul x)
       m-s/mean
       (m/div 2)))

(defn del-2-f-generator [x y]
  (fn [theta]
    (-> x
        m/transpose
        (m/mmul x)
        (m/div (m/row-count x)))))

(as-> "homework_ml_week_2/ex1data2.txt" $
      (io/resource $)
      (io/reader $)
      (line-seq $)
      (map #(clojure.string/split % #",") $)
      (m/emap r/read-string $)
      (def ex1-data-2 $))

(defn compute-cost [x y theta]
  (-> (m/mmul x theta)
      (m/sub y)
      (m/pow 2)
      m-s/mean
      (/ 2)))

;; feature normalize
(defn fn-on-col [f m]
  (->> m
       m/transpose
       (map f)))

(defn feature-normalize [x]
  (-> x
      (m/sub (fn-on-col m-s/mean x))
      (m/div (fn-on-col m-s/sd x))))

(let [x-to-be-norm (m/select ex1-data-2 :all [0 1])
      y (m/select ex1-data-2 :all 2)
      x (m/join-along 1 (m/broadcast 1 [(count ex1-data-2) 1]) (feature-normalize x-to-be-norm))
      target-f (partial compute-cost x y)]

  (search-convergence-point target-f [1 1 1] :del-f (del-f-generator x y) :del-2-f (del-2-f-generator x y)))

(let [x-to-be-norm (m/select ex1-data-2 :all [0 1])
      y (m/select ex1-data-2 :all 2)
      x (m/join-along 1 (m/broadcast 1 [(count ex1-data-2) 1]) (feature-normalize x-to-be-norm))
      target-f (partial compute-cost x y)]

  (search-convergence-point target-f [1 1 1]))

