(ns apps.matrix
  (:require [clojure.tools.logging :as logging]
            [clojure.core.matrix :as m]
            [clojure.core.matrix.stats :as m-s]))


(defn del
  "
  impl del in math via samplar.
  E.g. 1
    f: [x y z] -> v
    (del f): [x y z] -> [v1 v2 v3]

  E.g. 2
    f: [x1 x2 x3 ... xm] -> [y1 y2 y3 ... yn]
    (del f): [x1 x2 x3 ... xm] -> (n colum, m row matrix)

  If default dx is too small, some matrix inverse will get error. Tested in one matrix minimal value is 1E-2.
  "
  [f & {:keys [dx]
        :or {dx 1E-2}}]
  (fn [X]
    (-> dx
        (m/broadcast [(count X)])
        m/diagonal-matrix
        (m/add X)
        ((partial map f))
        (m/sub (f X))
        (m/div dx))))

;; ======================================================================
;; del examples
(def target-fn
  (fn [[x1 x2 x3]]
    (+ (* x1 2)
       (* x2 2)
       (* x3 2))))

(def gradient-fn
  (del target-fn))

(def hession-fn
  (del gradient-fn))

(gradient-fn [100 200 300])
(hession-fn [100 200 300])
;; ======================================================================


(defn search-convergence-point [f X-0
                                & {:keys [del-f del-2-f max-iter method]
                                   :or {max-iter 200
                                        method :newton-raphson
                                        del-f (del f)
                                        del-2-f (del del-f)}
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
