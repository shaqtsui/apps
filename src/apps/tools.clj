(ns apps.tools
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [apps.dcs :refer [as]]
            [taoensso.timbre :as timbre]
            [net.cgrand.enlive-html :refer :all]
            [postal.core :refer :all])
  (:import java.net.URL))


(defn wrap-spy [f]
  (fn [& more]
    (timbre/info "Invoking" f "with arguments:" more)
    (apply f more)))

(defn unzip
  "`url` can be anything valid for `io/input-stream`, default target path is `.`"
  ([url path]
   (-> url
       io/input-stream
       java.util.zip.ZipInputStream.
       (doto (as $
                 (doall (map (partial io/copy $)
                             (-> $
                              (as $ (partial (memfn getNextEntry) $))
                              repeatedly
                              (as $ (take-while (comp not nil?) $))
                              (as $ (filter (comp not (memfn isDirectory)) $))
                              (as $
                                  (map
                                   (comp #(doto % io/make-parents)
                                         (partial io/file path)
                                         (memfn getName))
                                   $)))))))
       .close))

  ([url]
   (unzip url ".")))


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

(defn send-mail-to-me
  [msg]
  (send-message {:host "smtp.163.com"
                 :user "xfcjscn"
                 :pass "Sara1011"}
                {:from "xfcjscn@163.com"
                 :to "xfcjscn@163.com"
                 :subject "MSG from Clojure"
                 :body msg}))

(defn rp1-stock []
  (-> "http://pur.store.sony.jp/digital-paper/products/DPT-RP1/DPT-RP1_purchase/"
      URL.
      html-resource
      (select [:a.s5-shippingLabel])
      first
      :content
      first))

#_(rp1-stock)

#_(def f
    (future
      (while true
        (let [status (rp1-stock)]
          (println status)
          (if (= "入荷待ち" status)
            (send-mail-to-me status))
          (Thread/sleep 10000)))))

#_(future-cancel f)

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

