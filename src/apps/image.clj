(ns apps.image
  (:require [mikera.image.core :as img]
            [mikera.image.colours :as imgc]
            [taoensso.timbre :as timbre]
            [apps.matrix :as a-m]
            [clojure.core.matrix :as m]
            ))


(m/set-current-implementation :vectorz)

(defn grayscale->grba
  "This is fast but `matrix->img` is elegant
  Operate on 1 img, all component values range from 0 to 1
  this img can be vector/matrix of :persistent-vector, as f of emap need to retrun vector"
  [Grayscale]
  (let [min-g (m/emin Grayscale)
        max-g (m/emax Grayscale)
        g-distance (- max-g min-g)]
    (m/emap (fn [g]
              (let [g-norm (-> g
                               (- min-g)
                               (/ g-distance))]
                [g-norm g-norm g-norm 1]))
            (m/coerce :persistent-vector Grayscale))))

(defn grayscale->img
  "Opt on 1 img, transpose matrix to enable matrix join to achieve img join"
  [Grayscale [w h]]
  (-> (m/reshape Grayscale
                 [w h])
      m/transpose
      grayscale->grba
      (->> (m/coerce :buffered-image))))


(defn matrix->img
  "Matrix is of r * c"
  [m]
  (let [min-v (m/emin m)
        max-v (m/emax m)
        v-distance (- max-v min-v)]
    (-> m
        (m/sub min-v)
        (m/div v-distance)
        (a-m/broadcast-on (m/dimensionality m)
                          [3])
        ((partial m/join-along
                  (m/dimensionality m)) (m/broadcast 1 (conj (m/shape m)
                                                             1)))
        (->> (m/coerce :buffered-image)))))


(defn join-img
  [imgs [r c]]
  "r - row, c - column, ignore redaunt items"
  (apply m/join-along 0
         (map #(apply m/join-along 1 %)
              (take r (partition c imgs)))))
