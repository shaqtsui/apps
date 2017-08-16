(ns apps.quil-example-3
  (:require [quil.core :as q]
            [quil.middleware :as q-m]))


(defn draw [state]
  (q/background 255)
  (q/lights)
  (q/fill 150 100 150)
  (q/sphere 75)
  (doseq [pos [[150 0 0] [-150 0 0]
               [0 150 0] [0 -150 0]
               [0 0 150] [0 0 -150]]]
    (q/with-translation pos
      (q/box 75))))


(q/defsketch my-sketch
  :draw draw
  :size [500 500]
  :renderer :p3d
  :middleware [q-m/fun-mode q-m/navigation-3d])
