(ns apps.cider-test)


(println "test")

(defn my-double [x]
  (* x 2))

(*
   (+ 10 20)
   (my-double
    (- 10 3)))

(-> 1
    my-double
    (- 1))

(+ 1 1)

my-double


