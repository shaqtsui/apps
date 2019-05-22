(ns apps.ml-w8
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.tools.reader :as r]
            [taoensso.timbre :as timbre]
            [clojure.core.matrix :as m]
            [clojure.core.matrix.stats :as m-s]
            [clojure.core.matrix.linear :as m-l]
            [clojure.data.csv :as csv]
            [apps.image :as a-i]
            [apps.matrix :as a-m]
            [mikera.image.core :as img]
            [clojure.math.numeric-tower :as math]
            [incanter.core :as i]
            [incanter.charts :as i-c]
            [taoensso.tufte :as tufte :refer [defnp fnp]]
            ))


(m/set-current-implementation :vectorz)

(def X (-> "ml_w8/dataX.csv"
           io/resource
           io/reader
           csv/read-csv
           (->> (m/emap r/read-string))
           m/matrix))

(def plot (doto  (i-c/scatter-plot (m/select X :all 0)
                                   (m/select X :all 1))
            i/view))


(def cs (a-m/k-means X
                     3))

(i-c/add-points plot
                (map #(m/select % 0) (:centroids cs))
                (map #(m/select % 1) (:centroids cs)))



(def groups (map (partial map last)
                 (vals (group-by first (map vector (:indexes cs) X)))))



(def plot1 (doto (i-c/scatter-plot (map #(m/select % 0) (first groups))
                                   (map #(m/select % 1) (first groups)))
             i/view))

(i-c/add-points plot1
                (map #(m/select % 0) (second groups))
                (map #(m/select % 1) (second groups)))

(i-c/add-points plot1
                (map #(m/select % 0) (last groups))
                (map #(m/select % 1) (last groups)))




(def bird (img/load-image-resource "ml_w8/bird_small.png"))
(def emacs6 (img/load-image-resource "ml_w8/emacs6.png"))
(def baidu (img/load-image-resource "ml_w8/bd_logo1.png"))

(defnp img-compress [b-img color-n]
  (let [[w h c] (m/shape b-img)]
    (-> b-img
        m/matrix
        (m/reshape [(* w h) c])
        (a-m/k-means color-n)
        (as-> $
            (map #(m/select (:centroids $)
                            %
                            :all)
                 (:indexes $)))
        (m/reshape [w h c])
        (->> (m/matrix :buffered-image)))))

(img/show bird)
#_(img/show (img-compress bird 16))
#_(def baidu-m (img-compress baidu 16))

(tufte/add-basic-println-handler! {})

(tufte/profile
 {}
 (def emacs6-m (img-compress emacs6 16))
 )



(def X-pca (-> "ml_w8/dataPCAX.csv"
               io/resource
               io/reader
               csv/read-csv
               (->> (m/emap r/read-string))
               m/matrix))

(def norm (a-m/feature-normalize X-pca))

(def X-norm (:norm norm))

(def plot-pca (scatter-plot X-pca X-norm))

(def cov (m/mul (/ 1
                   (m/row-count X-norm))
                (m/mmul (m/transpose X-norm)
                        X-norm)))

(def cov-svd
  (m-l/svd cov))

cov-svd

(def dir (m/mmul (:U cov-svd)
                 (m/diagonal-matrix (:S cov-svd))))

(def l1 [[0 0]
         (m/select dir :all :first)])

(def l2 [[0 0]
         (m/select dir :all :last)])


(def l3 [(:mu norm)
         (m/add (:mu norm)
                (m/select dir :all :first))])

(def l4 [(:mu norm)
         (m/add (:mu norm)
                (m/select dir :all :last))])

(def l5 [(:mu norm)
         (m/add (:mu norm)
                (m/mul (:sigma norm)
                       (m/select dir :all :first)))])

(def l6 [(:mu norm)
         (m/add (:mu norm)
                (m/mul (:sigma norm)
                       (m/select dir :all :last)))])


(add-lines plot-pca l1 l2 l3 l4)

(add-lines plot-pca l5 l6)


(def X-new-basis (m/mmul X-norm
                         (:U cov-svd)))

X-new-basis

;; change to old basis
(m/mmul (:U cov-svd)
        (m/select X-new-basis
                  :first
                  :all))

(m/select X-norm
          :first
          :all)

;; approxi restore
(def restored (map #(m/mul (m/select (:U cov-svd)
                                     :all
                                     :first)
                           %)
                   (m/select X-new-basis
                             :all
                             :first)))

(scatter-plot restored)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Face

(def X-face (-> "ml_w8/dataFaceX.csv"
                io/resource
                io/reader
                csv/read-csv
                (->> (m/emap r/read-string))
                m/matrix))


(def X-imgs (map #(a-i/grayscale->img % [32 32])
                 X-face))


(img/show (a-i/join-img X-imgs [10 10]))

(def norm (a-m/feature-normalize X-face))

(def X-pca (a-m/pca (:norm norm)))

(def X-norm-imgs (map #(a-i/grayscale->img % [32 32])
                 (:norm norm)))

(def egien (map #(a-i/grayscale->img % [32 32])
                (m/transpose (:U X-pca))))

(img/show (a-i/join-img X-norm-imgs [10 10]))
(img/show (a-i/join-img egien [6 6]))


(def projected-res (a-m/pca-project (:norm norm)
                                    (:U X-pca)
                                    100))


(def recovered-data (a-m/pca-recover projected-res (:U X-pca)))


(def recovered-img (map #(a-i/grayscale->img % [32 32])
                        recovered-data))

(img/show (a-i/join-img recovered-img [10 10]))
