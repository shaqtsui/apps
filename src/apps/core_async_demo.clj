(ns apps.core-async-demo
  (:require [clojure.core.async :as a]))

;; the heart is about chan & go
;; <!! - blocking take, base on blocking promise
;; <! - parking take, 

;; create channel
(a/chan)

(def c (a/chan 10))



;; blocking put & get
(a/>!! c "hello")

(a/<!! c)

(a/close! c)



;; non-blocking put & get `<!`, `>!` only able to used inside go block
;; go asyc execute its body and return a result channel, than use block get to get data
(a/<!! (a/go (println "executed in go")))
(a/<!! (a/go 4))

(def c2 (a/chan 10))

(a/go (a/>! c2 "H2"))

(a/<!! (a/go (a/<! c2)))



;; get from mul chans via alts!!
(def c3 (a/chan))
(def c4 (a/chan))

;; thread execute body in another REAL thread & return a chan for result
(a/thread (while true
            (let [[v ch] (a/alts!! [c3 c4])]
              (println "Read" v "from" ch))))


(a/>!! c3 "Hi")
(a/>!! c4 "World")



;; combine & go
(def c5 (a/chan))
(def c6 (a/chan))

(a/go (while true
        (let [[v c] (a/alts! [c5 c6])]
          (println "Get" v "from" c))))


(a/go (a/>! c5 "Hiii"))

(a/go (a/>! c6 "Wiii"))



;; go is lightweight not bound to thread
(def n 1000)
(def cs (repeatedly n a/chan))
(doseq [c cs]
  (a/go (a/>! c "Hii")))

(dotimes [i n]
  (let [[v c] (a/alts!! cs)]
    (println v)))



;; timeout chan
(let [t 200
      c (a/timeout t)
      begin (System/currentTimeMillis)]
  (a/<!! c)
  (println "Waited" (- (System/currentTimeMillis) begin) "ms"))



;; timed chan wait
(let [c (a/chan)
      begin (System/currentTimeMillis)]
  (a/alts!! [c (a/timeout 200)])
  (println "Waited chan" c "for" (- (System/currentTimeMillis) begin) "ms"))



;; get from any chan
(def c7 (a/chan))
(def c8 (a/chan))
(a/go (a/>! c7 "c7-1")
    (a/>! c8 "c8-1")
    (a/>! c7 "c7-2")
    (a/>! c8 "c8-2"))




;; put & get with call back (not recommended)
(def c9 (a/chan))
(a/put! c9 "C9")
(a/take! c9 #(println %))



;; mult
(def c10 (a/chan))
(def mu (a/mult c10))

(def c11 (a/chan))
(a/tap mu c11)
(def c12 (a/chan))
(a/tap mu c12)

(a/go (println (a/<! c11)))
(a/go (println (a/<! c12)))

(a/go (a/>! c10 "Multed Msg"))

