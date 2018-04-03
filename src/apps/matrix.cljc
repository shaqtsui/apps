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
#_(defn target-fn
    [X]
    (-> X
        (m/pow 2)
        (m-s/sum)))

#_(defn target-fn-vector-valued
    [X]
    (-> X
        (m/mul 2)))

#_(def gradient-fn
    (del target-fn))

#_(def hessian-fn
    (del gradient-fn))

#_(def jacobian-fn
    (let [gradient-fn (del target-fn-vector-valued)]
      #(m/transpose (gradient-fn %))))

#_(gradient-fn [100 200 300])
#_(hessian-fn [100 200 300])
#_(jacobian-fn [1 2 3])
;; ======================================================================

(defn fmin
  "List all keys so invoker can know supported parameters.
  Declare default in :or so invoker can know which parameter is optional"
  [f X-0
   & {:keys [gradient-fn hessian-fn max-iter method alpha plugin]
      :or {max-iter 200
           method :gradient-desent
           alpha 1E-2
           gradient-fn (del f)
           hessian-fn (del gradient-fn)}
      :as opts}]
  (and plugin
       (plugin f X-0 opts))

  (if (zero? max-iter)
    {:X X-0 :y (f X-0)}
    (recur f
           (m/sub X-0
                  (case method
                    :gradient-desent (m/mul (gradient-fn X-0)
                                            alpha)
                    :newton-raphson (m/mmul (m/inverse (hessian-fn X-0))
                                            (gradient-fn X-0))))
           (assoc opts :max-iter (dec max-iter)))))

(defn fmin-precision
  "List all keys so invoker can know supported parameters.
  Declare default in :or so invoker can know which parameter is optional"
  [f X-0
   & {:keys [gradient-fn hessian-fn precision method alpha plugin]
      :or {precision 1E-6
           method :gradient-desent
           alpha 1E-2
           gradient-fn (del f)
           hessian-fn (del gradient-fn)}
      :as opts}]
  (and plugin
       (plugin f X-0 opts))

  (let [gradient (gradient-fn X-0)
        iters (or (:iters opts)
                  0)]
    (if (== (m/esum (m/le (m/abs gradient)
                          precision))
            (m/ecount gradient))
      {:X X-0 :y (f X-0) :iters iters}
      (recur f
             (m/sub X-0
                    (case method
                      :gradient-desent (m/mul gradient
                                              alpha)
                      :newton-raphson (m/mmul (m/inverse (hessian-fn X-0))
                                              gradient)))
             (assoc opts :iters (inc iters))))))

(defn fmin-no-tail-opt [f X-0
                        & {:keys [gradient-fn hessian-fn max-iter method alpha]
                           :or {max-iter 200
                                method :gradient-desent
                                alpha 1E-2
                                gradient-fn (del f)
                                hessian-fn (del gradient-fn)}
                           :as opts}]
  (if (= max-iter 0)
    {:X X-0 :y (f X-0)}
    (m/sub (fmin-no-tail-opt f
                             X-0
                             (assoc opts :max-iter (dec max-iter)))
           (case method
             :gradient-desent (m/mul (gradient-fn X-0)
                                     alpha)
             :newton-raphson (m/mmul (-> X-0 hessian-fn m/inverse)
                                     (gradient-fn X-0))))))

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
