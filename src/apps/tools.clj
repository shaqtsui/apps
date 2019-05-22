(ns apps.tools
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [apps.dcs :refer [as]]
            [taoensso.timbre :as timbre]
            [net.cgrand.enlive-html :refer :all]
            [clojure.java.shell :as sh]
            [postal.core :refer :all]
            [dk.ative.docjure.spreadsheet :as sp]
            [datoteka.core :as fs]
            #_[cuerdas.core :as cstr])
  (:import java.net.URL))



(defn launchd
  "Space NOT supported in cmd"
  [cmd dir]
  (let [params (conj (str/split cmd #"\s+") :dir dir)
        _ (timbre/debug "invoke cmd:" params)
        res (apply sh/sh params)]
    (timbre/debug (update res :out str/split #"\r|\n"))
    (when-not (= 0 (:exit res))
      (timbre/debug "Retry after 30 sec sleep")
      (Thread/sleep (* 30 1000))
      (recur cmd dir)
      )))

#_(def res
    (launchd "./dl.sh" "/Volumes/Cheng/Cheng/math/Liberty/math421-fall-2018"))




(defn rename-files [dir]
  (-> dir
      fs/normalize
      (fs/list-dir "*.mp4")
      (->> (map (fn [f]
                  (let [old-name (fs/name f)]
                    
                    (fs/move f
                             (fs/path (fs/parent f)
                                      (str/replace-first old-name #"^(.+)-(mod.*)(.mp4)" "$2-$1$3")))))))))


#_(rename-files "/Volumes/Cheng/math/NPTEL/real-analysis")

#_(def res (->> (sp/load-workbook-from-file "/Users/fuchengxu/Downloads/管件%2B发货通知.xls")
                (sp/select-sheet "Sheet1")
                (sp/row-seq)
                (remove nil?)
                (map sp/cell-seq)
                (map #(map sp/read-cell %))
                rest rest butlast butlast
                (group-by #(clojure.string/join [(nth % 1) (nth % 2) (nth % 3) (nth % 6)]))
                (map (fn [ety]
                       (->> (val ety)
                            (reduce (fn [r r2]
                                      (update (vec  r) 5
                                              #(str (+ (Integer/parseInt %1)
                                                       (Integer/parseInt %2)))
                                              (nth r2 5)))))))
                

                ))

#_(->> res
(sp/create-workbook "By Clojure")
(sp/save-workbook-into-file! "RES.xlsx"))

(defn kaoji
"Buggy, cpu load not down after this completed"
([]
 (kaoji nil))
  ([t]
   (let [burn (future (last (repeatedly #(pmap inc (vec (range 10))))))]
     (println "Start kaoji...")
     (when t
       (Thread/sleep (* 1000 t))
       (future-cancel burn)
       (println "Burn" t "seconds, cancel result:"
                (future-cancelled? burn))))))


[dk.ative/docjure "1.12.0"]
(sh/sh "ls" :dir "/")

(defn aria-loop []
  (let [res (sh/sh "aria2c" "--log=/home/shark/aria.log" "--check-certificate=false" "https://ia802706.us.archive.org/zip_dir.php?path=/6/items/MIT6.006F11.zip&formats=MPEG4" :dir "/home/shark")]
    (if (= 0 (:exit res))
      res
      (do (doall (map println res))
          (println "Sleep 120 seconds............")
          (Thread/sleep (* 1000 120))
          (println "Start retry")
          (recur)))))

(def res (aria-loop))

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

