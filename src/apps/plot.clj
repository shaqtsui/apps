(ns apps.plot
  (:require [clojure.tools.logging :as logging]
            [quil.core :as q]
            [quil.middleware :as q-m]
            [clojure.core.matrix :as m]
            [clojure.core.matrix.stats :as m-s]))

(defn calculate-point
  "data is mandatory, as fn need data to determin its input"
  [data & {:keys [target-fn plot dim]
           :or {dim 2}}]
  (as-> data $
        ((if (nil? target-fn)
           identity
           (fn self [s-data]
             (if (-> s-data
                     m/shape
                     count
                     (= (if (> dim 2)
                          2
                          1)))
               (map (fn [input]
                      (conj (if (coll? input)
                              input
                              [input])
                            (target-fn input)))
                    s-data)
               (map self s-data)))) $)))

(def the-data (partition 10 (for [y (range -50 50 10)
                                  x (range -50 50 10)]
                              [x y])))


(def the-data-dim 3)

(def the-points (calculate-point the-data :target-fn #(m-s/sum %) :dim the-data-dim))

(def plotting-step 10)

;; this is not clojure sequence, it's entity
(def the-points-state-sequence (atom (partition-all plotting-step
                                                    (if (> the-data-dim 2)
                                                      (apply concat the-points)
                                                      the-points))))

(defn draw-lines
  "input example: [-100 0 0] [100 0 0] [200 0 0]"
  [& line-cords]
  (->> line-cords
       (partition 2 1)
       logging/spy
       (map (comp (partial apply q/line) concat))
       doall))

(defn draw-axis []
  (->> [[[-100 0 0] [100 0 0]]
        [[0 -100 0] [0 100 0]]
        [[0 0 -100] [0 0 100]]]
       (map (partial apply draw-lines))
       doall))


(def cam-position (atom [[0 0 0]
                         [0 0 0]
                         [0 1 0]]))

(defn setup []
  (q/frame-rate 1)
  (q/translate (/ (q/width) 2) (/ (q/height) 2))
  (q/stroke-weight 4)
  (draw-axis))

(defn draw []
  (when-let [pts (first @the-points-state-sequence)]
    (swap! the-points-state-sequence rest)
    (q/print-camera)
    (q/translate (/ (q/width) 2) (/ (q/height) 2))
    (-> (map (partial apply q/point)
             pts)
        doall)))

;; setup -> (update)@frame-rate
#_(def the-sketch (q/sketch
                 :setup setup
                 :draw draw
                 :renderer :p3d
                 :size [1000 600]
                 ;; customize (prevent error: error in process filter: nrepl--dispatch-response: [nREPL] No response handler with id nil found)
                 :features [:resizable :no-safe-fn :no-bind-output]))




(String. "ss")
(.toUpperCase "dd")



