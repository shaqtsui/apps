(ns apps.func-plot
  (:require ;; implict require macro
            [taoensso.timbre :as timbre]
            [clojure.core.matrix :as m]
            [clojure.core.matrix.stats :as m-s]
            [apps.matrix :as a-m]
            [dommy.core :as dommy]
            cljsjs.three
            cljsjs.three-orbitcontrols
            cljsjs.dat-gui
            ;;cljsjs.mathbox
            cljsjs.three-parametricgeometries))

(enable-console-print!)

(defn material []
  (new (. js/THREE -MeshLambertMaterial) (clj->js {:color 0x4080ff :transparent true :opacity 0.8 :side (. js/THREE -DoubleSide)})))

(defn geometry [math-func & {:keys [trans-func scalar offset slices stacks]
                             :or {scalar 5
                                  offset (-> scalar
                                             m/sub
                                             (m/div 2))
                                  slices 20
                                  stacks slices}}]
  (new (. js/THREE -ParametricGeometry)
       (comp #(new (. js/THREE -Vector3)
                   (first %)
                   (math-func %)
                   (second %))
             (or trans-func (comp (a-m/offset-func offset)
                                  (a-m/scale-func scalar)))
             butlast
             vector)
       slices
       stacks))

(defn cube [g m]
  (doto
   (new (. js/THREE -Mesh) g m)
    (aset "castShadow" true)
    (aset "receiveshadow" true)))

(defn renderer []
  (doto
   (new (. js/THREE -WebGLRenderer))
    (.setSize js/innerWidth js/innerHeight)
    (aset  "shadowMap" "enabled" true)
    (aset  "shadowMap" "type" (. js/THREE -PCFSoftShadowMap))
    (->> .-domElement (dommy/append! (dommy/sel1 :body)))))

(defn scene []
  (doto
   (new (. js/THREE -Scene))
    (.add (doto
           (new (. js/THREE -SpotLight) 0xffffff 1)
            (-> .-position (.set 35 40 35))
            (aset "angle" (/ Math/PI 4))
            (aset "castShadow" true)))
    (.add (new (. js/THREE -AmbientLight) 0xffffff 0.5))))

(defn camera []
  (doto
   (new (. js/THREE -PerspectiveCamera) 75 (/ js/innerWidth js/innerHeight) 0.1 1000)
    (aset "position" "z" 100)))

(defn render [renderer scene camera]
  (let [func #(.render renderer scene camera)]
    (doto
     (js/THREE.OrbitControls. camera)
      (dommy/listen! :change func))
    (doto
     (new (. js/dat -GUI))
      (.add camera "fov"))
    (func)))

(render (renderer)
        (doto
         (scene)
          (.add (cube (geometry #(-> % (m/pow 2) m-s/sum)) (material)))
          (.add (cube (geometry (constantly 0)) (material)))
          (.add (cube (geometry (constantly 1) :trans-func (comp a-m/cartesian-coord (a-m/scale-func [1 (* 2 Math/PI)]))) (material)))
          )
        (camera))

