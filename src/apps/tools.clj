(ns apps.tools
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [net.cgrand.enlive-html :refer :all])
  (:import java.net.URL))


;; replace file name from html page
(defn find-real-name-from-page [page-tree raw-name] 
  (as-> page-tree $
        (select $ [:a])
        (filter (comp string? first :content) $)
        (filter (comp #(str/ends-with? % raw-name) :href :attrs) $)
        (if (= 1 (count $))
          (-> $ first :content first (str "." (last (str/split raw-name #"\."))))
          nil)))

(defn folder-size [f] 
  (if (.isDirectory f)
    (apply + (pmap folder-size (.listFiles f)))
    (.length f)))


(defn format-size [l]
  (let [log-1024 (if (= 0 l) 
                   0
                   (-> l
                     Math/log
                     (/ (Math/log 1024))))]
    (str (format "%.1f" (/ l
                           (Math/pow 1024
                                     (int log-1024))))
         (-> ["B" "KB" "MB" "GB" "TB"]
             (nth log-1024)))))


#_(def c-size (->> "c:/Users/xfcjs/documents"
     io/file
     .listFiles
     (map #(vector (.getName %) (-> %
                                    folder-size
                                    format-size)))))


#_(clojure.pprint/pprint c-size)
       

#_(def page-tree (-> "http://www.huidengzhiguang.com//c/2016-01-08/843.shtml" URL. html-resource))

#_(->> "C:/Users/xfcjs/Downloads/佛教入门课程/2" io/file file-seq rest
       (map (fn [f]
              (-> f
                  (.renameTo (as-> f $
                                   (str (.getParent $) "/" (->> $ .getName (find-real-name-from-page page-tree)))
                                   (io/file $)))))))


;; extract files
#_(->> "F:/BaiduCloud" io/file file-seq rest
     (map (fn [f]
            (-> ["C:/Program Files/WinRAR/rar.exe" "x" "-pbmwhuiju" (log/spy (.getAbsolutePath f)) "F:/DTS"]
                ProcessBuilder.
                .inheritIO
                .start
                .waitFor))))

