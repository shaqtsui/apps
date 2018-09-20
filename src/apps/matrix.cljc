(ns apps.matrix
  (:require [taoensso.timbre :as timbre] ;; implicit require macro
            [clojure.core.reducers :as r]
            [clojure.core.matrix :as m]
            [clojure.core.matrix.stats :as m-s]
            [clojure.core.matrix.linear :as m-l]
            [apps.dcs :as dcs]
            [clojure.math.numeric-tower :as math]
            [taoensso.tufte :as tufte :refer [defnp fnp]]
            [incanter.core :as i]
            [incanter.charts :as i-c]

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

(defn broadcast-on
  "`dim` - target dimension
  e.g.
  (broadcast-on [[1 2] [3 4]] 2 [2]) -> [[[1.0,1.0]
                                          [2.0,2.0]]
                                         [[3.0,3.0]
                                          [4.0,4.0]]]"
  [m dim ex-shape]
  (if (zero? dim)
    (m/broadcast m
                 (concat ex-shape
                         (m/shape m)))
    (m/matrix (map #(broadcast-on %
                                  (dec dim)
                                  ex-shape)
                   m))))

(defn add-lines
  [plot & points]
  (last (map #(i-c/add-lines plot
                             (m/get-column % 0)
                             (m/get-column % 1))
             points)))

(defn add-points
  [plot & points]
  (last (map #(i-c/add-points plot
                              (m/get-column % 0)
                              (m/get-column % 1))
             points)))

(defn scatter-plot
  [& points]
  (let [plot (i-c/scatter-plot)]
    (when points
      (apply add-points plot points))
    (i/view plot)
    plot))

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

(defn del2
  "2 side approximate del via secant(finite difference approximation)
  different from `del` is that this support `X` as matrix not only vector
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
    (m/reshape (m/matrix (map (comp (fn [[pre-X next-X]]
                                      (m/div (m/sub (f next-X)
                                                    (f pre-X))
                                             (* 2 dx)))
                                    (fn [idx]
                                      (let [orig-x (first (m/select-indices X
                                                                            [idx]))
                                            pre-x (- orig-x dx)
                                            next-x (+ orig-x dx)]
                                        [(m/set-indices X
                                                        [idx]
                                                        [pre-x])
                                         (m/set-indices X
                                                        [idx]
                                                        [next-x])])))
                              (m/index-seq X)))
               (concat (m/shape X)
                       (m/shape (f X))))))

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

(defn round [X & {p :precision}]
  (if p
    (let [factor (math/expt 10 p)]
      (-> X
          (m/mul factor)
          m/round
          (m/div factor)))
    (m/round X)))

(defn fmin-plugin-error-detect
  "Throw exception when result not descending on rate `r`"
  [r]
  (let [store (atom nil)]
    (fn [f X-0 opts]
      (if-let [iters (:iters opts)]
        (when (-> iters
                  (mod r)
                  zero?)
          (let [y (f X-0)
                previous-y (peek @store)]
            (if (< y previous-y)
              (reset! store (conj @store y))
              (throw (Exception. (str "NOT descending!!!\n" previous-y " -> " y))))))
        (reset! store [(- ##Inf)])))))

(defn fmin-plugin-log
  "Print info on rate `r`"
  [r]
  (fn [f X-0 opts]
    (when-let [iters (:iters opts)]
      (when (-> iters
                (mod r)
                zero?)
        (timbre/debug "iters: " iters "\n" "y: " (f X-0))))))

(defn fmin-plugin-plot
  "Plot info on rate `r`"
  [r]
  (let [store (atom nil)]
    (fn [f X-0 opts]
      (if-let [iters (:iters opts)]
        (when (-> iters
                  (mod r)
                  zero?)
          (add-points @store [[iters (f X-0)]]))
        (reset! store (scatter-plot))))))

(defn fmin-plugin-last-x
  [r store]
  (fn [f X-0 opts]
    (if-let [iters (:iters opts)]
      (when (-> iters
                (mod r)
                zero?)
        (reset! store X-0))
      (reset! store nil))))

#_(defn root-fn
  "All search of convergence point of 2 lines can reduce to root problem(1 line with x-axis) "
  [& {:keys [gradient-fn precision method alpha plugin]
      :or {precision 1E-6
           method :gradient-desent
           alpha 1E-2
           gradient-fn (del f)}
      :as opts}]
  (fn [f & Xs]
    (let [iters (or (:iters opts)
                    0)
          Ys (apply f Xs)]
      (if (map m/maximum Ys)
        {:X X-0 :y (f X-0) :iters iters}
        (recur f
               (m/sub X-0
                      (case method
                        :gradient-desent (m/mul gradient
                                                alpha)
                        :newton-raphson (m/mmul (let [hess-inv (m/inverse (round (hessian-fn X-0)
                                                                                 :precision 6))]
                                                  #dbg ^{:break/when (nil? hess-inv)}
                                                  hess-inv)
                                                gradient)))
               (assoc opts :iters (inc iters)))))))

(defn fmin
  "List all keys so invoker can know supported parameters.
  Declare default in :or so invoker can know which parameter is optional
  `X-0` is a vector, as its easy to get its hessian matrix, and able to combine multiple vector into 1"
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
                      :newton-raphson (m/mmul (let [hess-inv (m/inverse (round (hessian-fn X-0)
                                                                               :precision 6))]
                                                #dbg ^{:break/when (nil? hess-inv)}
                                                hess-inv)
                                              gradient)))
             (assoc opts :iters (inc iters))))))

;; WARNING!!!
;; For N-dementional Array Confuse here:
;; [x y z] represent 1 item: [x y z] or 3 item: x y z
;; E.g. (add-constant-comp [x y z]) -> [[1 x] [1 y] [1 z]] or [1 x y z]
;;
;; Only element-wise opearte is not confuse here, as it always operate on most inner element

(defn poly-term
  "E.g. (poly-term [[x y z]] 2) ->
  x^2 | x^1 * y^1,  x^1 * z^1 | x^0 * y^2, x^0 * y^1 * z^1, x^0 * z^2
  Operate on dimention 1"
  [X degree]
  (if (== 0 degree)
    (m/broadcast (m/scalar-array 1)
                 [(m/row-count X)])
    (if (or (m/vec? X)
            (== 1
                (m/column-count X)))
      (m/pow X degree)
      (apply m/join-along
             1
             (map #(if (m/vec? %)
                     (m/column-matrix %)
                     %)
                  (map (fn [first-comp-degree]
                         ;; double tranpose to support auto broadcast on vector
                         (m/transpose (m/mul (m/pow (m/select X :all :first)
                                                    first-comp-degree)
                                             (m/transpose (poly-term (m/select X :all :rest)
                                                                     (- degree first-comp-degree))))))
                       (-> degree inc range reverse)))))))

(defn map-feature
  "Operate on dimention 1"
  [X degree]
  (apply m/join-along
         1
         (map #(if (m/vec? %)
                 (m/column-matrix %)
                 %)
              (map (partial poly-term X)
                   (range 1 (inc degree))))))

(defn mean-normalize
  "(m-s/mean normlized-X) -> 0
  Operate on dimention 1"
  [X]
  (let [mu (m-s/mean X)]
    {:mu mu
     :norm (m/sub X mu)}))


(defn feature-normalize
  "(m-s/mean normlized-X) -> 0
  (m-s/sd normalized-X) -> 1
  Operate on dimention 1"
  [X]
  (let [sigma (m-s/sd X)]
    (-> X
        mean-normalize
        (assoc :sigma sigma)
        (update :norm m/div sigma))))

(defn add-constant-comp
  "Operate on dimention 1"
  [X]
  (m/join-along 1
                (m/broadcast (m/scalar-array 1)
                             [(m/row-count X) 1])
                (if (m/vec? X)
                  (m/column-matrix X)
                  X)))

(defn linear-reg-hypo-fn [THETA]
  "(= (m/row-count THETA)
      (inc (m/column-count X))"
  (fn [X]
    (m/mmul (add-constant-comp X)
            THETA)))

(defn linear-reg-cost [THETA X Y lambda]
  (let [m (m/row-count X)]
    (+ (* (/ 1
             (* 2 m))
          (m-s/sum (m/pow (m/sub ((linear-reg-hypo-fn THETA) X)
                                 Y)
                          2)))
       (* (/ lambda
             (* 2 m))
          (m-s/sum (m/pow (m/select THETA
                                    :rest)
                          2))))))

(defn linear-reg-gradient [THETA X Y lambda]
  (let [m (m/row-count X)
        Y-hat ((linear-reg-hypo-fn THETA) X)
        X-w-const (add-constant-comp X)]
    (m/add (m/mul (/ 1 m)
                  (m-s/sum (m/mul (m/transpose (m/broadcast-like (m/transpose X-w-const)
                                                                 (m/sub Y-hat
                                                                        Y)))
                                  X-w-const)))
           (m/mul (/ lambda m)
                  (m/mset THETA 0 0)))))

(defnp cluster
  "Cluster used in K-means, result: [Ci Ci Cj Cj Ci ....]
  Independent func as may directly used by new data
  x can be identified only by index, as x may be duplicate
  E.g. (cluster [[1 2] [10 10] [2 2] [8 8]] [[1 1] [11 11]])
  "
  [X C]
  (pmap (fn [x]
          (first (apply min-key
                        (comp m/scalar
                              m-s/sum-of-squares
                              (partial m/sub x)
                              second)
                        (map-indexed vector C))))
        X))

;; fold parallel compute can only work on vector not list even realized
;; NOT parallel
#_(r/fold 5
          (r/monoid (fn [x y]
                      (println "combinef" x y)
                      (into x y))
                    (fn []
                      (println "get I")
                      []))
          (fn [xs x]
            (println "reducef" xs x)
            (conj xs x))
          (doall '(1 2 3 4 5 6 7 8)))

;; parallel
#_(r/fold 5
          (r/monoid (fn [x y]
                      (println "combinef" x y)
                      (into x y))
                    (fn []
                      (println "get I")
                      []))
          (fn [xs x]
            (println "reducef" xs x)
            (conj xs x))
          [1 2 3 4 5 6 7 8])

(defnp centroids-of-clusters
  "cluster-n can derived from c-idxes, but required as parameter to speed up the algorithm
  As pmap in cluster already parallel, disable parallel here"
  [X c-idxes cluster-n]
  (let [x-count-0 (vec (repeat cluster-n
                               0))
        x-sum-0 (vec (repeat cluster-n
                             (m/zero-vector (m/column-count X))))
        identity-element [x-sum-0 x-count-0]
        x-accu (r/fold (r/monoid (partial map m/add)
                                 (constantly identity-element))
                       (fn [[x-sum x-count] [c-idx x]]
                         [(update x-sum
                                  c-idx
                                  (partial m/add x))
                          (update x-count
                                  c-idx
                                  inc)])
                       (map vector c-idxes X))]
    (map m/div
         (first x-accu)
         (second x-accu))))

(defn optimize-cluster
  "iterate cluster"
  [X init-C]
  (let [c-idxes (cluster X init-C)
        new-C (centroids-of-clusters X
                                     c-idxes
                                     (m/row-count init-C))]
    (if (m/equals init-C new-C)
      {:indexes c-idxes :centroids init-C}
      (recur X new-C))))

(defnp k-means
  [X k]
  (let [init-C (take k (distinct (repeatedly (partial rand-nth
                                                      (seq X)))))]
    (optimize-cluster X init-C)))


(defnp cov
  "Coverence, [[x1^2 x2x1]
               [x1x2 x2^2]]
  "
  [X]
  (m/mul (/ 1
            (m/row-count X))
         (m/mmul (m/transpose X)
                 X)))

(defnp pca
  "find basis span max variances, and corresponding variance value"
  [X]
  (m-l/svd (cov X)))


(defnp pca-project
  "project to basis of max variance(first k columns of U)"
  [X U k]
  (m/mmul X
          (m/select U
                    :all
                    (range k))))

(defnp pca-recover
  "Z in basis of max variance(first k columns of U)"
  [Z U]
  (m/mmul Z
          (m/transpose (m/select U
                                 :all
                                 (range (m/column-count Z))))))


(defn single-var-gaussian
  "`X` is a vector or n * 1 matrix, this is rarely used, most case use `mul-var-gaussian` "
  [X mu sigma2]
  (m/mul (/ 1
            (math/sqrt (* 2 Math/PI sigma2)))
         (m/exp (m/sub (m/div (m/pow (m/sub X mu)
                                     2)
                              (* 2 sigma2))))))


(defn mul-var-gaussian
  "A surface whose double integral is 1, integral on 1 demention get the probabilities on the other demantion"
  [X Mu Cov]
  (m/mul (/ 1
            (* (math/expt (* 2 Math/PI)
                          (/ (m/column-count X)
                             2))
               (math/sqrt (m/det Cov))))
         (m/exp (m/sub (m/mul (/ 1 2)
                              (let [X-Mu (m/sub X Mu)]
                                ;; transpose X-Mu -> transform -> inner product
                                (m/matrix (map (comp m/scalar m/mmul)
                                               X-Mu
                                               (m/transpose (m/mmul (m/inverse Cov)
                                                                    (m/transpose X-Mu)))))))))))



(defn estimate-gaussian
  "`X`s gaussian distribute parameters.
  Sigma2 similar to m-s/variance, but variance divide total variance by m-1, which will make (m-s/variance [1]) -> ##NaN"
  [X]
  (let [mu (m-s/mean X)]
    {:mu mu
     :sigma2 (m-s/mean (m/pow (m/sub X mu)
                              2))}))


(defn true-positives
  "Terms base on Y-hat"
  [Y-hat Y]
  (m-s/sum (m/emap #(if (== 1 %1 %2)
                      1
                      0)
                   Y-hat
                   Y)))

(defn false-positives [Y-hat Y]
  (m-s/sum (m/emap #(if (and (== 1 %1)
                             (== 0 %2))
                      1
                      0)
                   Y-hat
                   Y)))

(defn false-negtives [Y-hat Y]
  (m-s/sum (m/emap #(if (and (== 0 %1)
                             (== 1 %2))
                      1
                      0)
                   Y-hat
                   Y)))

(defn precision [Y-hat Y]
  (let [tp (true-positives Y-hat Y)]
    (if (== 0 tp)
      0
      (/ tp
         (+ tp (false-positives Y-hat Y))))))

(defn recall [Y-hat Y]
  (let [tp (true-positives Y-hat Y)]
    (if (== 0 tp)
      0
      (/ tp
         (+ tp (false-negtives Y-hat Y))))))

(defn f1-score [Y-hat Y]
  (let [prec (precision Y-hat Y)
        rec (recall Y-hat Y)]
    (if (== 0 prec rec)
      0
      (/ (* 2 prec rec)
         (+ prec rec)))))

(defn select-threshold
  "`P` - probabilities, `Y` - 0 & 1s
  May contains different epsilon with same f1"
  [P Y]
  (let [min-p (m/minimum P)
        max-p (m/maximum P)
        step (/ (- max-p min-p)
                1000)]
    (apply max-key :f1
           (map (fn [epsilon]
                  {:epsilon epsilon
                   :f1 (f1-score (m/lt P epsilon)
                                 Y)})
                (concat (range min-p max-p step)
                        [max-p])))))

(defn anomalies
  "`epsilon` - threshold"
  [X P epsilon]
  (m/matrix (map first (filter  #(< (last %)
                                    epsilon)
                                (map vector X P)))))

(defnp matrixs->vector [& ms]
  (apply m/join-along 0 (map m/as-vector
                             ms)))

(defnp vector->matrixs [v & shapes]
  (if shapes
    (let [s (first shapes)
          [hv tv] (split-at (apply * s)
                            v)]
      (cons (m/reshape hv s)
            (apply vector->matrixs tv (rest shapes))))
    nil))

(defnp cofi-cost
  "`R` - 1, 0 matrix indicate where rated corresponding to Y
  all parameters except lambda are matrix not vector
  NO bias feature here
  Parallel compute here"
  ([THETA X R Y lambda]
   (apply + (pvalues (* (/ 1 2)
                        (m/esum (m/pow (m/mul (m/sub (m/mmul X
                                                             (m/transpose THETA))
                                                     Y)
                                              R)
                                       2)))
                     (* (/ lambda 2)
                        (m/esum (m/pow THETA
                                       2)))
                     (* (/ lambda 2)
                        (m/esum (m/pow X
                                       2))))))
  ([THETA-X theta-shape x-shape R Y lambda]
   (let [[THETA X] (vector->matrixs THETA-X theta-shape x-shape)]
     (cofi-cost THETA X R Y lambda))))

(defnp cofi-gradient-theta
  [THETA X R Y lambda]
  (m/add (m-s/sum (m/mul (broadcast-on (m/mul (m/sub (m/mmul X
                                                             (m/transpose THETA))
                                                     Y)
                                              R)
                                       2
                                       [(m/column-count X)])
                         (broadcast-on X 1
                                       [(m/column-count Y)])))
         (m/mul lambda
                #_(m/set-column THETA
                                0
                                (m/zero-vector (m/row-count THETA)))
                THETA)))


(defnp cofi-gradient-x
  [THETA X R Y lambda]
  (m/add (m-s/sum (m/mul (broadcast-on (m/transpose (m/mul (m/sub (m/mmul X
                                                                          (m/transpose THETA))
                                                                  Y)
                                                           R))
                                       2
                                       [(m/column-count X)])
                         (broadcast-on THETA
                                       1
                                       [(m/row-count Y)])))
         (m/mul lambda
                X)))

(defnp cofi-gradient
  "`THETA-X` is a vector of THETA & X rolled & joined, result is the same
  Parallel computation here."
  [THETA-X theta-shape x-shape R Y lambda]
  (let [[THETA X] (vector->matrixs THETA-X theta-shape x-shape)]
    (matrixs->vector (cofi-gradient-theta THETA X R Y lambda)
                     (cofi-gradient-x THETA X R Y lambda))))

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
  (apply m/join-along 0 (map m/as-vector
                             Xs)))

(defn roll [X shapes]
  "Convert a long vector(may infinit) to a sequences matrix of shape)
  e.g. (roll % (map m/shape THETAs))"
  (if (seq shapes)
    (let [[m n] (first shapes)
          [_ r] (split-at (* m n)
                          X)]
      (cons (m/reshape X
                       [m n])
            (roll r (rest shapes))))
    []))
