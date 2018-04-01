(ns apps.ml-w5
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.tools.reader :as r]
            [taoensso.timbre :as timbre]
            [clojure.core.matrix :as m]
            [clojure.core.matrix.stats :as m-s]
            [clojure.core.matrix.operators :as mo]
            [apps.image :as a-i]
            [apps.matrix :as a-m]
            [mikera.image.core :as img]
            [clojure.math.numeric-tower :as math]
            [incanter.core :as i]
            [incanter.charts :as i-c]))

(m/set-current-implementation :vectorz)

(def X-v (-> "ml_w5/data.csv"
             io/resource
             io/reader
             line-seq
             (->> (map #(s/split % #","))
                  (m/emap r/read-string))))

(def X (-> X-v
           m/matrix
           m/transpose))

(def Y-v (-> "ml_w5/datay.csv"
             io/resource
             io/file
             io/reader
             line-seq
             (->>
              (m/emap r/read-string))))

(defn label [X]
  (let [I (-> X
              seq
              distinct
              count
              m/identity-matrix)]
    (-> X
        (->> (map #(m/select I (- % 1) :all)))
        m/matrix
        m/transpose)))

(defn label->value [X]
  (m/matrix (map (fn [X-v]
                 (+ 1
                    (.indexOf (seq X-v)
                              (m/emax X-v))))
               (m/columns X))))

(def Y (label Y-v))

#_(def X-imgs (map #(a-i/seq->img % [20 20] (m/emin X) (m/emax X))
                   (m/transpose X)))

#_(-> X-imgs
      (a-i/merge-imgs 100)
      img/show)

(defn ACTs
  "X contains multiple vecotr of x in math formatter, perform x transform via base THETAs"
  [X THETAs]
  (reduce (fn [res THETA]
            (conj res
                  (m/logistic (m/mmul THETA
                                      (m/join-along 0
                                                    (m/broadcast (m/scalar-array 1)
                                                                 [1 (m/column-count (last res))])
                                                    (last res))))))
          [X]
          THETAs))

(defn ACTs-w-bias
  "X contains multiple vecotr of x in math formatter, perform x transform via base THETAs"
  [ACTs]
  (map #(m/join-along 0
                      (m/broadcast (m/scalar-array 1)
                                   [1 (m/column-count %)])
                      %)
       ACTs))

(defn nn-hypo-fn
  [THETAs]
  (fn [X]
    (-> X
        (ACTs THETAs)
        last)))

(defn nn-cost [THETAs X Y lambda]
  (let [Y-hat ((nn-hypo-fn THETAs) X)]
    (+ (* (- (/ 1
                (m/column-count X)))
          (m-s/sum (m-s/sum (mo/+ (mo/* Y
                                        (m/log Y-hat))
                                  (mo/* (mo/- 1 Y)
                                        (m/log (mo/- 1 Y-hat)))))))
       (* (/ lambda
             (* 2 (m/column-count X)))
          (-> THETAs
              (->> (map (comp m/esum #(m/pow % 2) #(m/select % :all :rest)))
                   (reduce +)))))))

(defn DELTA-last [ACT-last Y]
  (->
   (m/sub ACT-last Y)))

(defn DELTA-i [DELTA-j ACT-i THETA-i]
  (mo/* (m/mmul (m/transpose (m/select THETA-i :all :rest))
                DELTA-j)
        (mo/* ACT-i
              (mo/- 1 ACT-i))))

(defn DELTAs [ACTs Y THETAs]
  (reduce (fn [res [ACT-i THETA-i]]
            (cons (DELTA-i (last res)
                           ACT-i
                           THETA-i)
                  res))
          [(DELTA-last (last ACTs)
                       Y)]
          (map vector
               (->  ACTs
                    butlast
                    rest)
               (rest THETAs))))

(defn nn-gradient [THETAs X Y lambda]
  (let [ACTs (-> X
                 (ACTs THETAs))
        ACTs-w-bias (ACTs-w-bias ACTs)]

    (map (fn [ACT-w-bias-i DELTA-j THETA-i]
           (-> (map #(m/outer-product %2 %1)
                    (m/columns ACT-w-bias-i)
                    (m/columns DELTA-j))
               (->> (reduce m/add))
               (mo// (m/column-count X))
               (as-> $
                     (if (= 0 lambda)
                       $
                       (mo/+ $
                             (m/join-along 1
                                           (m/broadcast (m/scalar-array 0)
                                                        [(m/row-count THETA-i) 1])
                                           (mo/* (m/select THETA-i :all :rest)
                                                 (/ lambda
                                                    (m/column-count X)))))))))
         (-> ACTs-w-bias
             butlast)
         (DELTAs ACTs Y THETAs)
         THETAs)))

(def THETA-1 (-> "ml_w5/t1.csv"
                 io/resource
                 io/reader
                 line-seq
                 (->> (map #(s/split % #","))
                      (m/emap r/read-string))
                 m/matrix))

(def THETA-2 (-> "ml_w5/t2.csv"
                 io/resource
                 io/reader
                 line-seq
                 (->> (map #(s/split % #","))
                      (m/emap r/read-string))
                 m/matrix))

(def THETAs [THETA-1 THETA-2])

(nn-cost THETAs X Y 1)

(-> nn-gradient
    (apply [THETAs X Y 1])
    (->> (map m/esum)
         (reduce +)))

#_(def nn-gradient-del (a-m/del
                        #(nn-cost (a-m/roll % (map m/shape THETAs))
                                  (m/select X :all [1 500])
                                  (m/select Y :all [1 500])
                                  1)))

#_(-> (nn-gradient-del (a-m/unroll THETAs))
      (->> (map m/esum)
           (reduce +)))

(def store (atom []))
(def monitor
  (fn [f X-0 opts]
    (when (-> opts
              :max-iter
              ((fnil mod 0) 50)
              zero?)
      (let [y (f X-0)
            previous-y (last @store)]
        (timbre/debug "max-iter: " (:max-iter opts) "\n" "y: " y)
        (if (or (nil? previous-y)
                (< y previous-y))
          (reset! store (conj @store y))
          (do
            (reset! store [])
            (throw (Exception. (str "NOT descending!!!\n" previous-y " -> " y)))))))))

(def p (a-m/fmin #(nn-cost (a-m/roll % (map m/shape THETAs))
                           X
                           Y
                           1)
                 (a-m/unroll THETAs)
                 #_(:X p)
                 :gradient-fn #(a-m/unroll (nn-gradient (a-m/roll % (map m/shape THETAs))
                                                        X
                                                        Y
                                                        1))
                 :alpha 2
                 :plugin monitor
                 :max-iter 1000))

#_(i/view (i-c/scatter-plot (range (count @store)) @store))

(def optimized-THETAs
  (a-m/roll (:X p) (map m/shape THETAs)))

(nn-cost optimized-THETAs
         X
         Y
         1)

(def opt-fn (nn-hypo-fn optimized-THETAs))

(-> (opt-fn X)
    label->value)

(m-s/sum (m/eq (label->value Y)
               (-> (opt-fn X)
                   label->value)))

(/ 4941.0 5000)

(-> (map (comp m/emax) (m/columns (opt-fn X)))
    (->> (apply +))
    (/ 5000))

