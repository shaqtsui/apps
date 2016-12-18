(ns apps.datomic-exmaple
  (:require [datomic.api :as d]))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(def uri "datomic:mem://seettle")
(d/create-database uri)

(def conn (d/connect uri))

(def schema-file-path "C:/Users/xfcjs/Downloads/datomic-pro-0.9.5359/samples/seattle/seattle-schema.edn")

(def schema-tx (read-string (slurp schema-file-path)))

#_@(d/transact conn schema-tx)

(def data-file-path "C:/Users/xfcjs/Downloads/datomic-pro-0.9.5359/samples/seattle/seattle-data0.edn")

(def data-tx (-> data-file-path slurp read-string))

(def data-file-2-path "C:/Users/xfcjs/Downloads/datomic-pro-0.9.5359/samples/seattle/seattle-data1.edn")

(def data-2-tx (-> data-file-path slurp read-string))

#_@(d/transact conn data-tx)

(def result (->>
  conn
  d/db
  (d/q '[:find (pull ?c [*]) :where [?c :community/name]])
  ffirst
  :community/neighborhood
  :db/id
  (d/entity (-> conn d/db))
  :community/_neighborhood
  (map keys)))

#_(def result-1 (->>
                conn d/db
                (d/q '[:find [?n ...] :where [?c :community/name ?n] [?c :community/type :community.type/twitter]])
                ))

(def rules [
            '[[twitter ?c] [?c :community/type :community.type/twitter]]
            '[[region ?c ?r] [?c :community/neighborhood ?n]
                [?n :neighborhood/district ?d]
                [?d :district/region ?re]
                [?re :db/ident ?r]]
            '[[social-media ?c] [?c :community/type :community.type/twitter]]
            '[[social-media ?c] [?c :community/type :community.type/facebook-page]]
            '[[northern ?c] [region ?c :region/new]]
            '[[northern ?c] [region ?c :region/n]]
            '[[northern ?c] [region ?c :region/nw]]
            '[[southern ?c] [region ?c :region/sw]]
            '[[southern ?c] [region ?c :region/s]]
            '[[southern ?c] [region ?c :region/se]]])
