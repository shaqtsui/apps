(ns apps.incanter-exmaple
  (:require [clojure.tools.logging :as log]
            [clojure.java.io :as io]
            [incanter.core :refer :all]
            [incanter.stats :refer :all]
            [incanter.charts :refer :all]
            [incanter.io :refer :all]
            [incanter.datasets :refer :all]
            [incanter.pdf :refer :all]))


(-> 1000
    sample-normal
    histogram
    view)

(view (function-plot cos -10 10))

(dataset [1 2 3] [1 2 3])

(to-dataset [[1 2 3]
             [2 3 3]])

(conj-cols [1 2 3] [3 3 3] [2 2 2])

(conj-rows [1 2 3] [3 3 3] [2 2 2])

(read-dataset "https://raw.githubusercontent.com/incanter/incanter/master/data/cars.csv" 
              :header true)

(-> :cars get-dataset 
    (save "./my-test-data.csv"))


(->> :cars get-dataset
    ($ :speed))

(def cars-data (get-dataset :cars))

(def iris-data (get-dataset :iris))

cars-data

iris-data

(with-data cars-data
  [(mean ($ :speed)) (sd ($ :speed))])


(with-data iris-data
  #_(view $data)
  (view ($ 1 [:Sepal.Length :Sepal.Width])))

($where {:Species "setosa"} iris-data)

($where {:Sepal.Length {:lt 5 :gt 4.5}
         :Species {:in #{"setosa" "virginica"}}} iris-data)

($where (fn [row]
          (or (< ((log/spy row) :Sepal.Width) 4)
              (> (row :Sepal.Length) 5)))
        iris-data)


($order [:speed :dist] :desc cars-data)

(->> iris-data 
     ($rollup mean :Sepal.Length :Species)
     ($order :Sepal.Length :desc) ) 

;; plot
(view (scatter-plot '(1 2 3) '(1 2 3 4)))

(view (scatter-plot ($ :Sepal.Width iris-data) ($ :Sepal.Length iris-data) :group-by ($ :Species iris-data)))

(save-pdf
  (scatter-plot :Sepal.Width :Sepal.Length :data iris-data) "my-pdf.pdf")

(view (xy-plot ($ :Sepal.Width iris-data) ($ :Sepal.Length iris-data)))

(doto (function-plot sin -10 10)
  (add-text 0 0 "text ...")
  (add-function cos 1 5)
  view)

(def a [[1 2 3] [4 5 6] [7 8 9]])
(def b (repeat 3 (repeat 3 2)))

a
b
(plus a b)

(mult a b)

(mult a 2)

(trans a)

(mmult a b)

(identity-matrix 3)

