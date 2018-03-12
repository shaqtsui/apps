#lang racket

(substring "abcedefghij" 2 4)


(let ((x 1)
      (y 2))
  (println x))


(andmap string? '("ss" "b" "c"))

(ormap number? '("a" "b" 1))


(foldl (lambda (elem v)
	 (+ v (* elem elem)))
       0
       '(1 2 3))


empty


(define (my-length lst)
  (cond
   [(empty? lst) 0]
   [else (+ 1 (my-length (rest lst)))]))

(my-length '(1 2 3))


(define (my-map f lst)
  (cond
   [(empty? lst) empty]
   [else (cons (f (first lst))
	       (my-map f (rest lst)))]))

(my-map string? '("a" "b" 1))

(cons 1 2)

(cons 3 (cons 1 2))

(pair? (cons 1 2))

(car (cons 1 2))

(cdr (cons 1 2))


(cons 1 2)

(+ 1 . (2))

(1 . < . 2)

(2 . + . 3)

(+ 1 . 2)


(boolean? #f)

(if "no" 1 2)

#e0.5

(inexact->exact 0.1)


(+ 1+2i 3)

(= 1 1.0)

(eqv? 1 1.0)

(= 1/2 0.5)

(= 1/10 0.1)

(inexact->exact 0.1)
(inexact->exact 0.5)

#\A

(integer->char 17)

#\space


(display #\A)

(char=? #\a #\A)

(char-ci=? #\a #\A)

"\u03BB"


(string-ref "adf" 1)

(first '(1 2))

(byte? 1)

(byte? 256)

#"Apple"

(bytes-ref #"Apple" 0)

(make-bytes 3 65)


(bytes->string/utf-8 #"\316\273")
(bytes->string/latin-1 #"\316\273")


(string->symbol "apple")
(string->keyword "apple")

'#:apple

(pair? null)
(list? null)
(pair? empty)
(list? empty)


(eq? empty null)


(write "d")
(display "d")

(for-each display '(1 2 3))
(map display '(1 2 3))


(member "b" '("a" "b" "c"))

(assoc 'where '((when "3:00") (who "Shark") (where "SH")))

(define p (mcons 1 2))

p

(pair? p)

(mpair? p)

(car p)

(mcar p)

; vector
#(a b)
#("a" "b")

(vector-ref #("a" "b" "c") 2)


(vector->list #("d" "c"))


; hash table

(define ht (make-hash))
(hash-set! ht 'apple 'red)
(hash-ref ht 'apple)

(define ht2 (hash 'apple 'red 'banana 'yello))

(hash-ref ht2 'apple)

(define ht3 (hash-set ht2 'coconut 'brown))

(hash-ref ht2 'coconut)
(hash-ref ht3 'coconut)

#hash(("a" . "b") ("c" . "d"))

(define ht4 (make-weak-hash))

ht4


;box
(define b (box "a"))

b

(unbox b)

(set-box! b "c")

b

; void
(void 1 2)

; #<undefined> result of a reference(variable?) whose value not yet available


