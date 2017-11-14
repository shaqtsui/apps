(ns apps.hello-world
  (:require cljsjs.three
            cljsjs.three-orbitcontrols))

(enable-console-print!)


(println "Hello world")


;;;;;;;;;;;;;;;;;;;;;;;;;;; common ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; render scene with camera
(def renderer (new (. js/THREE -WebGLRenderer)))
(. renderer setSize js/innerWidth js/innerHeight)
(.. js/document -body (appendChild (. renderer -domElement)))
(def scene (new (. js/THREE -Scene)))
(def aspect (/ js/innerWidth js/innerHeight))
(def camera (new (. js/THREE -PerspectiveCamera) 75 aspect 0.1 1000))
(aset camera "position" "z" 100)



;;;;;;;;;;;;;;;;;;;;;;;;;; box ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(def p-camera (js/THREE.PerspectiveCamera. 75 (/ js/innerWidth js/innerHeight) 0.1 1000 ))
;;
;;(def geometry (js/THREE.BoxGeometry. 1 1 1 ))
;;
;;(def material (js/THREE.MeshBasicMaterial. (clj->js {:color 0x00ff00})))
;;
;;(def cube (js/THREE.Mesh. geometry material))
;;
;;(.add scene cube)
;;
;;(aset p-camera "position" "z" 5)
;;
;;
;;(defn animate []
;;  (.requestAnimationFrame js/window animate)
;;  (aset cube "rotation" "x" (+ 0.1 (aget cube "rotation" "x")))
;;;;  (aset cube "rotation" "y" (+ 0.1 (aget cube "rotation" "y")))
;;  (.render renderer scene p-camera))
;;
;;
;;(animate)



;;;;;;;;;;;;;;;;;;;;;;;; line ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(def p-camera (new (. js/THREE -PerspectiveCamera) 45 (/ js/innerWidth js/innerHeight) 1 500))
;;(.. p-camera -position (set 0 0 100))
;;(. p-camera lookAt (new (. js/THREE -Vector3) 0 0 0))
;;
;;(def geometry (new (. js/THREE -Geometry)))
;;
;;(.. geometry -vertices (push (new (. js/THREE -Vector3) -10 0 0)))
;;(.. geometry -vertices (push (new (. js/THREE -Vector3) 0 10 0)))
;;(.. geometry -vertices (push (new (. js/THREE -Vector3) 10 0 0)))
;;
;;(def material (new (. js/THREE -LineBasicMaterial) (clj->js {:color 0x0000ff})))
;;
;;(def line (new (. js/THREE -Line) geometry material))
;;
;;(. scene add line)
;;
;;(. renderer render scene p-camera)



;;;;;;;;;;;;;;;;;;;;;;;; begin with 3d webgl part 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(def camera (new (. js/THREE -PerspectiveCamera) 75 aspect 0.1 1000))
;;
;;(aset camera "position" "z" 100)
;;
;;(def geometry (new (. js/THREE -BoxGeometry) 20 20 20))
;;
;;(def material (new (. js/THREE -MeshLambertMaterial) (clj->js {:color 0xfd59d7})))
;;
;;(def cube (new (. js/THREE -Mesh) geometry material))
;;
;;(. scene add cube)
;;
;;(def light (new (. js/THREE -PointLight) 0xFFFF00))
;;
;;(.. light -position (set 10 0 25))
;;
;;(. scene add light)
;;
;;
;;
;;
;;(defn animate []
;;  (js/requestAnimationFrame animate)
;;  (aset cube "rotation" "x" (+ 0.1 (.. cube -rotation -x)))
;;  (aset cube "rotation" "y" (+ 0.1 (.. cube -rotation -y)))
;;;;  (. camera updateProjectionMatrix)
;;  (. renderer render scene camera)
;;  )
;;
;;
;;(animate)
;;


;;;;;;;;;;;;;;;;;;;;;;;; begin with 3d webgl part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def geometry (new (. js/THREE -BoxGeometry) 20 20 20))
(def material (new (. js/THREE -MeshLambertMaterial) (clj->js {:color 0xfd59d7})))
(def cube (new (. js/THREE -Mesh) geometry material))
(. scene add cube)

;;(def light (new (. js/THREE -PointLight) 0xffff00))
;;(def light (new (. js/THREE -AmbientLight) 0xffff00))
;;(def light (new (. js/THREE -HemisphereLight) 0xffff00))
(def light (new (. js/THREE -SpotLight) 0xffff00))
(.. light -position (set 10 0 25))
(. scene add light)

(def controls (new (. js/THREE -OrbitControls) camera))

(defn animate []
  (js/requestAnimationFrame animate)
  (aset cube "rotation" "x" (+ 0.02 (.. cube -rotation -x)))
  (aset cube "rotation" "y" (+ 0.01 (.. cube -rotation -y)))
  (. controls update)
  (. renderer render scene camera))
(animate)
