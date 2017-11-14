(ns apps.func-plot
  (:require [clojure.core.matrix :as m]
            [clojure.core.matrix.stats :as m-s]
            cljsjs.three
            cljsjs.three-orbitcontrols
            cljsjs.dat-gui
            ;;cljsjs.mathbox
            cljsjs.three-parametricgeometries))

(enable-console-print!)

;;;;;;;;;;;;;;;;;;;;;;;;;;; common ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; render scene with camera
(def renderer (new (. js/THREE -WebGLRenderer)))
(. renderer setSize js/innerWidth js/innerHeight)
(aset renderer "shadowMap" "enabled" true)
(aset renderer "shadowMap" "type" (. js/THREE -PCFSoftShadowMap))
(.. js/document -body (appendChild (. renderer -domElement)))

(def scene (new (. js/THREE -Scene)))
;;(def light (new (. js/THREE -PointLight) 0xffff00))
(def spot-light (new (. js/THREE -SpotLight) 0xffffff 1))
(.. spot-light -position (set 35 40 35))
(aset spot-light "angle" (/ Math/PI 4))
(aset spot-light "castShadow" true)
;;(aset spot-light "shadow" "mapSize" "width" 500)
;;(aset spot-light "shadow" "mapSize" "height" 500)
;;(aset spot-light "shadow" "camera" "near" 10)
;;(aset spot-light "shadow" "camera" "far" 200)
(. scene add spot-light)

(def ambient-light (new (. js/THREE -AmbientLight) 0xffffff 0.5))
;;(. scene add ambient-light)
(def hemi-light (new (. js/THREE -HemisphereLight) 0xffffff 0xffffff 0.5))
(. scene add hemi-light)

(def box-geometry (new (. js/THREE -BoxGeometry) 200 5 200))
(def box-material (new (. js/THREE -MeshPhongMaterial) (clj->js {:color 0x808080})))
(def box-cube (new (. js/THREE -Mesh) box-geometry box-material))
(.. box-cube -position (set 0 -20 0))
(aset box-cube "receiveShadow" true)
(. scene add box-cube)

(def aspect (/ js/innerWidth js/innerHeight))
(def camera (new (. js/THREE -PerspectiveCamera) 75 aspect 0.1 1000))
(aset camera "position" "z" 100)

(def controls (new (. js/THREE -OrbitControls) camera))
(aset controls "enableDamping" true)
;;(aset controls "autoRotate" true)

(defn animate []
  (js/requestAnimationFrame animate)
  (. controls update)
  (. renderer render scene camera))

;;;;;;;;;;;;;;;;;;;;;;;;; dat-gui ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def options (clj->js {:speed 12}))
(def gui (new (. js/dat -GUI)))
(. gui add options "speed"  -10 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;(def geometry (new (. js/THREE -ParametricGeometry) (.. js/THREE -ParametricGeometries -klein) 40 40))
;;(def material (new (. js/THREE -MeshBasicMaterial) (clj->js {:color 0x00ff00})))
;;(def cube (new (. js/THREE -Mesh) geometry material))
;;
;;(. scene add cube)
;;(. scene add light)
;;
;;(defn animate []
;;  (js/requestAnimationFrame animate)
;;  ;; update controls if camera changed via other channel, or controls need to be update by time(auto rotate)
;;  ;; required if controls.enableDamping or controls.autoRotate are set to true
;;  (. controls update)
;;  (. renderer render scene camera)
;;  )
;;
;;(animate)
;;
;;


;;(def geometry (new (. js/THREE -Geometry)))
;;(.. geometry -vertices (push
;;                        (new (. js/THREE -Vector3) -10 10 0)
;;                        (new (. js/THREE -Vector3) -10 -10 0)
;;                        (new (. js/THREE -Vector3) 10 -10 0)))
;;
;;(.. geometry -faces (push (new (. js/THREE -Face3) 0 1 2)))


;;(def geometry (new (. js/THREE -BoxGeometry) 20 20 20))


(defn math-func [X]
  (-> X
      (m/pow 2)
      m-s/sum))

(defn wrap-three [func]
  (fn [X]
    (new (. js/THREE -Vector3)
         (first X)
         (func X)
         (second X))))

;; x - [0 1], y - [min-y max-y]
(defn linear-trans-func-factory [min-y max-y]
  (fn [x]
    (-> x
        (* (- max-y min-y))
        (+ min-y))))

(defn pre-func-factory [min-x1 max-x1 min-x2 max-x2]
  (let [u-trans-func (linear-trans-func-factory min-x1 max-x1)
        v-trans-func (linear-trans-func-factory min-x2 max-x2)]
    (fn [u v]
      [(u-trans-func u)
       (v-trans-func v)])))


(def geometry (new (. js/THREE -ParametricGeometry) (comp (wrap-three math-func) (pre-func-factory -5 5 -5 5)) 200 200))

(def material (new (. js/THREE -MeshLambertMaterial) (clj->js {:color 0x4080ff :transparent true :opacity 0.8 :side (. js/THREE -DoubleSide)})))

(def cube (new (. js/THREE -Mesh) geometry material))
(aset cube "castShadow" true)
(. scene add cube)

(animate)
