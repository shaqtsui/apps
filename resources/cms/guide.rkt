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







