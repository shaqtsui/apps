(ns apps.image
  (:require [mikera.image.core :as img]
            [mikera.image.colours :as imgc]
            [taoensso.timbre :as timbre]
            [apps.tools :refer [wrap-spy]]))

(defn gray->grb [grayscale]
  (-> grayscale
      ((juxt #(bit-shift-left % 16) #(bit-shift-left % 8) identity))
      (->> (cons 0xff000000)
           (apply bit-or))))

(defn seq->img [xs size min-grayscale max-grayscale]
  (-> (img/new-image (first size) (second size))
      (doto
       (as-> $
             (img/set-pixels $ (-> $
                                   img/get-pixels
                                   (doto
                                    (as-> $
                                          (doall (map
                                                  #(aset $ %1 %2) ;; type mismatch error when invoke aset via apply
                                                  (range)
                                                  (map #(-> %
                                                            (- min-grayscale)
                                                            (/ (- max-grayscale min-grayscale))
                                                            (* 255)
                                                            -
                                                            (+ 255)
                                                            (Math/round)
                                                            gray->grb)
                                                       xs)))))))))
      (img/rotate 90)
      (img/flip :horizontal)))

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
