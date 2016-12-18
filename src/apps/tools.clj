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

#_(def page-tree (-> "http://www.huidengzhiguang.com//c/2016-01-08/843.shtml" URL. html-resource))

#_(->> "C:/Users/xfcjs/Downloads/佛教入门课程/2" io/file file-seq rest
       (map (fn [f]
              (-> f
                  (.renameTo (as-> f $
                                   (str (.getParent $) "/" (->> $ .getName (find-real-name-from-page page-tree)))
                                   (io/file $)))))))


;; extract files
(->> "F:/BaiduCloud" io/file file-seq rest
     (map (fn [f]
            (-> ["C:/Program Files/WinRAR/rar.exe" "x" "-pbmwhuiju" (log/spy (.getAbsolutePath f)) "F:/DTS"]
                ProcessBuilder.
                .inheritIO
                .start
                .waitFor))))

