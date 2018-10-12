(ns apps.parse-java
  (:require [clojure.java.io :as io]))


;; code
(def jc (javax.tools.ToolProvider/getSystemJavaCompiler))

(def sjfm (.getStandardFileManager jc nil nil nil))

(def fo (.getJavaFileObjectsFromFiles sjfm [(io/file "/Users/fuchengxu/gitrepo/apps/src/apps/Helloworld.java")]))

(def res (first (.parse (.getTask jc nil nil nil nil nil fo))))
(.getEndPosition res)
(.getTypeDecls res)

(.startPosition (first (.getTypeDecls res)))


;; doc
(def dt (javax.tools.ToolProvider/getSystemDocumentationTool))

;; call will generate html directly
#_(.call (.getTask dt nil nil nil nil nil fo))




          
