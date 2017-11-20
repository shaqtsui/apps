(ns apps.matrix
  (:require [taoensso.timbre :as timbre]
            [clojure.core.matrix :as m]
            [clojure.core.matrix.stats :as m-s]))

;; timber debug not work in chrome, as console.debug do nothing

(defn del
  "
  approximate del via secant(finite difference approximation)
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
(defn target-fn
  [[x1 x2 x3]]
  (+ (* x1 2)
     (* x2 2)
     (* x3 2)))

(defn target-fn-vector-valued
  [[x1 x2 x3]]
  [(-> (* x1 x1)
       (- (* x1 2))
       (+ (* x2 x2))
       (- x3)
       (+ 1))
   (-> (* x1 x2 x2)
       (- x1)
       (- (* 3 x2))
       (+ (* x2 x3))
       (+ 2))
   (-> (* x1 x3 x3)
       (- (* 3 x3))
       (+ (* x2 x3 x3))
       (+ (* x1 x2)))])

(def gradient-fn
  (del target-fn))

(def hessian-fn
  (del gradient-fn))

(def jacobian-fn
  (let [gradient-fn (del target-fn-vector-valued)]
    #(m/transpose (gradient-fn %))))

(gradient-fn [100 200 300])
(hessian-fn [100 200 300])
(jacobian-fn [1 2 3])
;; ======================================================================


(defn search-convergence-point [f X-0
                                & {:keys [gradient-fn hessian-fn max-iter method]
                                   :or {max-iter 200
                                        method :newton-raphson
                                        gradient-fn (del f)
                                        hessian-fn (del gradient-fn)}
                                   :as opts}]
  (let [y (f X-0)]
    (if (or
         (= max-iter 0)
         (= (timbre/spy y) 0))
      {:X X-0 :y y :iter (- 200 max-iter)}
      (let [hessian (hessian-fn X-0)
            hessian-inverse (m/inverse hessian)]
        (if (= nil hessian-inverse)
          {:X X-0 :y y :iter (- 200 max-iter) :error (format "Inverse of hessian is nil. hessian is: %s" hessian)}
          (recur f
                 (m/sub X-0
                        (m/mmul hessian-inverse
                                (gradient-fn X-0)))
                 (assoc opts :max-iter (dec max-iter))))))))



(defn search-root [f X-0
                   & {:keys [jacobian-fn max-iter method]
                      :or {max-iter 200
                           method :newton-raphson
                           jacobian-fn (let [gradient-fn (del f)]
                                         #(m/transpose (gradient-fn %)))}
                      :as opts}]
  (let [y (f X-0)]
    (if (or
         (= max-iter 0)
         (m/zero-matrix? (timbre/spy y)))
      {:X X-0 :y y :iter (- 200 max-iter)}
      (let [jacobian (jacobian-fn X-0)
            jacobian-inverse (m/inverse jacobian)]
        (if (= nil jacobian-inverse)
          {:X X-0 :y y :iter (- 200 max-iter) :error (format "Inverse of jacobian is nil. jacobian is: %s" jacobian)}
          (recur f
                 (m/sub X-0
                        (m/mmul jacobian-inverse y))
                 (assoc opts :max-iter (dec max-iter))))))))

(defn cartesian-coord
  "polar-coord in format: [r theta]"
  [polar-coord]
  (-> polar-coord
      timbre/spy
      last
      ((juxt m/cos m/sin))
      (m/mul (first polar-coord))))

(defn scale-func [scalar]
  #(-> scalar
       timbre/spy
       (m/broadcast [(count %)])
       m/diagonal-matrix
       (m/mmul %)))


(defn offset-func [offset]
  #(-> offset
       timbre/spy
       (m/broadcast [(count %)])
       (m/add %)))


