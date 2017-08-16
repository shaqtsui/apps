(ns apps.quil-example-2
  (:require [quil.core :as q]
            [quil.middleware :as q-m]))


(def min-r 10)


(defn setup []
  {:x 0 :y 0 :r min-r})


(defn update [state]
  (update-in state [:r] inc))


(defn draw [state]
  (q/background 255)
  (q/ellipse (:x state) (:y state) (:r state) (:r state)))

(defn shrink [r]
  (max min-r (dec r)))

(defn mouse-moved [state event]
  (-> state
      (assoc :x (:x event) :y (:y event))
      (update-in [:r] shrink)))


(q/defsketch example
  :size [200 200]
  :setup setup
  :draw draw
  :update update
  :mouse-moved mouse-moved
  :middleware [q-m/fun-mode])
