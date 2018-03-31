(ns apps.matrix
  (:require [taoensso.timbre :as timbre] ;; implicit require macro
            [clojure.core.matrix :as m]
            [clojure.core.matrix.stats :as m-s]
            [apps.dcs :as dcs]
            ;; load nd4clj to register implementation explicitly, as core.matrix/KNOWN-IMPLEMENTATIONS have wrong namespace configed
            #_[nd4clj.matrix]))

;; to get timber debug work in chrome, chrome's console need to have Verbose selected


;; data type:
;; scalar - number
;; zero-dimentional array - ? (created by: scalar-array, can get the value via: scalar)
;; one-dimentional array - vector
;; two-dimensional array - matrix
;; 3+-dimensional array - ?

;; matrix implementation can be a lot of stuff
;; matrix implementation setting
;; here will impact matrix contruct func. e.g. (matrix [[1 2]])
;; i.e.
;; code: (m/mul [1 2] [1 2])
;; you hard coded persistent vector matrix, this setting have no impact on it
;; code: (m/mul (matrix [1 2]) (matrix [1 2]))
;; matrix constructed via matrix with vector as input will be impacted
;; note: some implementation only support 1 or 2 dimentaional data e.g. clatrix

;; (matrix 1) -> 1.0 (for vectorz, clatrix, persistent-vector doublearray)
;;            -> #object[xxxNDArryxxx] (for ndarray, ndarray-double)

;; broadcast may retrun a semantic wrapper(impl: NDWrapper, PersistentVector) over the original matrix
;; for external impl(vectorz, clatrix) its not a wrapper
;; for internal impl(number, ndarray, ndarray-double, double-array, persistent-vector) it's a wrapper

;; join-along first argument - result mapping:
;; semantic wrapper - semantic wrapper(persistent vector)
;; x - x

;; benchmark on: (-> ts nn-hypo-fn (apply [X]) class time)
;; "Elapsed time: 252.466507 msecs"
;; mikera.matrixx.Matrix
;; "Elapsed time: 1236.258934 msecs"
;; clatrix.core.Matrixp
;; "Elapsed time: 7561.123261 msecs"
;; clojure.core.matrix.impl.ndarray_object.NDArray
;; "Elapsed time: 5561.062206 msecs"
;; clojure.core.matrix.impl.ndarray_double.NDArrayDouble
;; "Elapsed time: 9177.468293 msecs"
;; clojure.lang.PersistentVector
;; for nd4j:
;; "Elapsed time: 79882.531356 msecs"
;; clojure.core.matrix.impl.ndarray_object.NDArray
;; for :double-array, I can not get the result in 30 minuites

;; external impl
(m/set-current-implementation :vectorz)
#_(m/set-current-implementation :clatrix)
;; core.matrix internal impl
#_(m/set-current-implementation :ndarray)
#_(m/set-current-implementation :ndarray-double)
#_(m/set-current-implementation :persistent-vector)
#_(m/set-current-implementation :double-array)
;; this impl is buggy: e.g. (mmul m1 m2) -> instance of NDArray
#_(m/set-current-implementation :nd4j)

(def KNOWN-IMPLEMENTATIONS
  "A map of known core.matrix implementation namespaces.

   core.matrix will attempt to load these namespaces when an array of the specified
   keyword type is requested."
  (array-map
   :vectorz 'mikera.vectorz.matrix-api
   :vectorz-opencl 'mikera.vectorz.opencl-api
   :neanderthal 'uncomplicate.neanderthal.impl.matrix-api
   :clojure 'clojure.core.matrix.impl.clojure
   :ndarray 'clojure.core.matrix.impl.ndarray-object
   :ndarray-double 'clojure.core.matrix.impl.ndarray-double
   :ndarray-float 'clojure.core.matrix.impl.ndarray
   :ndarray-long 'clojure.core.matrix.impl.ndarray
   :persistent-vector 'clojure.core.matrix.impl.persistent-vector
   :persistent-map 'clojure.core.matrix.impl.sparse-map
   :sequence 'clojure.core.matrix.impl.sequence
   :double-array 'clojure.core.matrix.impl.double-array
   :scalar-wrapper 'clojure.core.matrix.impl.wrappers
   :slice-wrapper 'clojure.core.matrix.impl.wrappers
   :nd-wrapper 'clojure.core.matrix.impl.wrappers
   :dataset 'clojure.core.matrix.impl.dataset
   :jblas :TODO
   :clatrix 'clatrix.core
   :parallel-colt :TODO
   :ejml :TODO
   :nd4j 'nd4clj.kiw
   :ujmp :TODO
   :weka 'clj-ml.matrix-api
   :commons-math 'apache-commons-matrix.core
   :mtj 'cav.mtj.core.matrix
   :aljabr 'thinktopic.aljabr.core))

(defn del
  "2 side approximate del via secant(finite difference approximation)
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
        (->> (repeat (m/row-count X)))
        m/diagonal-matrix
        (dcs/parellel
         (m/add X)
         (-> m/sub
             (m/add X)))
        (->> (map (comp m/matrix
                        (partial map f)))
             (reduce m/sub))
        (m/div (* 2 dx)))))

;; ======================================================================
;; del examples
(defn target-fn
  [X]
  (-> X
      (m/pow 2)
      (m-s/sum)))

(defn target-fn-vector-valued
  [X]
  (-> X
      (m/mul 2)))

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


(defn unroll [Xs]
  "Convert sequence of matrix to a flatten vector"
  (apply m/join-along 0 (map m/to-vector
                             Xs)))


(defn roll [X shapes]
  "Convert a long vector(may infinit) to a sequences matrix of shape)
  e.g. (roll % (map m/shape THETAs))"
  (if (seq shapes)
    (let [[m n] (first shapes)
          [c r] (split-at (* m n)
                          X)]
      (cons (m/matrix (partition-all n c))
            (roll r (rest shapes))))
    []))
