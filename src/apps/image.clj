(ns apps.image
  (:require [mikera.image.core :as img]
            [mikera.image.colours :as imgc]))


(defn convert-to-grb [grayscale]
  (let [a 0xff000000
        g (bit-shift-left grayscale 16)
        r (bit-shift-left grayscale 8)
        b grayscale]
    (bit-or a g r b)))


(defn convert-to-img [x img-min-grayscale img-max-grayscale]
  (map (fn [xi]
         (let [target-img (img/new-image 20 20)
               pixels (img/get-pixels target-img)]
           (dotimes [i 400]
             (aset pixels i (-> (nth xi i)
                                (- img-min-grayscale)
                                (/ (- img-max-grayscale img-min-grayscale))
                                (* 255)
                                -
                                (+ 255)
                                (Math/round)
                                convert-to-grb)))     
           (img/set-pixels target-img pixels)
           (-> target-img
               (img/rotate 90)
               (img/flip :horizontal))))
       x))

(defn merge-imgs [imgs col-number]
  (let [sample-img (first imgs)
        width  (-> sample-img 
                   img/width
                   (* col-number))
        height (-> sample-img
                   img/height
                   (* (-> imgs
                          count
                          (/ col-number)
                          inc)))
        target-img (img/new-image width height)
        graphic (.getGraphics target-img)
        locations (for [y (range 0 height (img/height sample-img))
                        x (range 0 width (img/width sample-img))]
                    [x y])]
    (doall (map (fn [src-img loc]
                  (.drawImage graphic src-img (first loc) (last loc) nil))
                imgs
                locations))
    target-img))
