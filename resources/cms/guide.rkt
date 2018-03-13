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

(apply + 10 '(1 2))


keyword-apply

((lambda x x) 1 2 3)


(define f1 (lambda (x . xs)
	     (apply max (cons x xs))))


(f1 1)


(define greet
  (lambda (given [surname "Xu"])
    (string-append "Hello, " given " " surname)))

(greet "Shark" "C")

(define greet2
  (lambda (#:hi [hi "Hello"] given #:last surname)
    (string-append hi " " given " " surname)))


(greet2 "Shark" #:last "C")

(greet2 #:last "C" "Shark")

(greet2 "Shark" #:hi "Hi," #:last "Xu")

(define greet3
  (case-lambda
    [(name) (string-append "Hello, " name)]
    [(given surname) (string-append "Hello, " given " " surname)]))


(greet3 "Shark")

(greet3 "Shark" "Xu")

(greet3)

(define ((make-and-suffix s2) s)
  (string-append s s2))

(make-and-suffix "D")
((make-and-suffix "D") "S")


; multipl values					;

(quotient/remainder 13 3)

(define (split-name name)
  (define parts (regexp-split " " name))
  (values (list-ref parts 0) (list-ref parts 1)))


(split-name "Shark S")

(string-append (split-name "Shar S"))
(string-append "s" "d")
(values 1 2)


(let ([me "Shark"]
      [you "Sara"]
      )
  (let ([me you]
	[you me])
    (list me you)))

(let ([me "Shark"]
      [you "Sara"]
      )
  (let* ([me you]
	[you me])
    (list me you)))

(let* ([name "Shark"]
       [name (cons "A" name)]
       [name (cons "B" name)])
  name)

(let ([name "Shark"])
  (let ([name (cons "A" name)])
    (let ([name (cons "B" name)])
      name)))


(letrec ([rev (lambda (l)
		(if (empty? l)
		    empty
		    (append (rev (cdr l))
				 (list (first l)))))])
  (rev '(1 2 3)))


(define (after-s lst)
  (cond [(member "s" lst) => cdr]
	[else (error "not there")]))



(after-s '(1 2))

(after-s '("a" "b" "s" "e" "o"))



(begin (println "1")
       (println "2"))


(begin0 1
  (println "a"))


(define greeted null)

(define (greet name)
  (set! greeted (cons name greeted))
  (string-append "Hello, " name))


(greet "S")
(greet "D")

greeted

(case 7
  [(0) 'zero]
  [(1) 'one]
  [(2) 'two]
  [(3 4 5) 'many])


(parameterize ([error-print-width 8])
  (car (expt 10 1024)))


(error-print-width)


(define location (make-parameter "here"))

(location)

(parameterize ([location "there"])
  (list 
   (location)
   (parameterize ([location "in house"])
     (location))
   (location)))


(location "there")


; if change on a installed parameter, it's only a temp change
(parameterize ([location "there"])
  (list (location)
	(begin (location "river")
	       (location))
	(location)))

(location)

(define v "init")
(define v1 "init")
(define v2 "init")


(parameterize ([location "t1"])
  (thread (lambda ()
	    (location "t2")
	    (set! v2 (location))))
  (sleep 2)
  (set! v (location)))

(parameterize ([location "t1"])
  (location "t2")
  (set! v2 (location))
  (sleep 2)
  (set! v (location)))

(begin
  (location "t1")
  (thread (lambda ()
	    (set! v1 (location))
	    (location "t2")
	    (set! v2 (location))))
  (sleep 2)
  (set! v (location)))

v
v1
v2


(struct posn (x y) #:transparent)




(define p1 (posn 1 2))

(define p2 (struct-copy posn p1 [x 3]))
p1
p2
(struct-info p1)
(struct-info p2)
(posn-x p2)
(posn-y p2)


(struct 3d-posn posn (z))
; defines 3d-posn, 3d-posn?, 3d-posn-z

(define p3 (3d-posn 1 2 3))


(posn? p3)
(3d-posn? p3)

(posn-x p3)
(posn-y p3)

(3d-posn-z p3)

(struct lead (w h))

(equal? (lead 1 2) (lead 1 2))

(equal? (posn 1 2) (posn 1 2))
p1
#(struct:posn 2 3)

;prefab struct type




; sprout: create a data type named sprout, have 1 field
; sprout-kind: access 1st field
(struct sprout (kind) #:prefab)


#s(sprout 1)


(define lunch #s(sprout bean))

; only create struct-type which operate on old data type(field access by position)
(struct sprout (type) #:prefab)

(sprout? lunch)

(sprout-type lunch)
(sprout-kind lunch)

(sprout 'garlic)

; sprout: create a data type named sprout, have 3 filed
; sprout-kind: access 1st field
; sprout-yummy?: access 2nd field
; sprout-count: access 3rd field
; sprout? check the data is of type named sprout & have 3 field 
(struct sprout (kind yummy? count) #:prefab)
#s(sprout 1 2 3)
(sprout? lunch)




(struct building (rooms [location #:mutable]) #:prefab)

(struct house building ([occupied #:auto]) #:prefab #:auto-value 'no)

(house 5 'SH)

(building-location (house 3 'CN))
(house-occupied (house 2 'SG))

(struct thing (name)
	#:transparent
	#:guard (lambda (name type-name)
		  (cond [(string? name) (string->symbol name)]
			[(symbol? name) name]
			[else (error type-name
				     "bad name: ~e"
				     name)])))


(thing-name (thing
	     "dd"))

(thing-name (thing 'd))

(thing 1)


(struct person thing (age)
	#:transparent
	#:guard (lambda (name age type-name)
		  (if (negative? age)
		      (error type-name "bad age: ~e" age)
		      (values name age))))


(person 'd 1)

(person 1 -1)

(person 1 1)

(struct greeter (name)
	#:property prop:procedure (lambda (self other)
				    (string-append "Hi "
						   other
						   ", I'm "
						   (greeter-name self))))


(define s (greeter "Shark"))

(s "Sara")




; modules
(require setup/dirs)


(find-collects-dir)

(module name-id
  initial-module-path
  decl ...)
; name-id match the file name
; initial-module-path import bootstraps the syntax in the body

(module cake racket
  (provide print-cake)
  (printf "Init Cake")
  (define (print-cake n)
    (print n)))

; currently it's a unassociated module
; refer unassociated module:
(require 'cake)

print-cake


#lang racket
decl ...
expand to:
(module name racket
  decl ...)
Note: name derived from file name


; submodule
#lang racket
; outer module can refer sub module
(module sub-guide racket
  (provide tiger)
  (define tiger "DD"))


; sub module can refer outer module
(module* sub-guide2 racket
  (provide tiger)
  (define tiger posn))

; main submodule(declared via module or module* or module+) required automaticly when outer module runed (not required)
(module* main #f
  (s "dd"))


; multiple sub module have same name, combined into one
(module+ test
  (s 1))

(module+ test
  (s 3))


; module path
(module m (lib "racket/date.rkt")
  (println "dd"))

; download from planet server
(require (planet schematics/random:1/random))

(require (submod 'cake inner-sub-module))

(require (submod "." cake))


; imports
(require (only-in 'm great))
(require (only-in 'm [great g]))
(require (except-in 'm great))
(require (prefix-in m: (except-in 'm ghost)))
(require (except-in (prefix-in m: 'm) m:ghost))
; in module - introduce bindings
; in top level - introduce bindings & instantiates module




; export
(export (rename-out [lunch g-lunch]))
(export (struct-out posn))
(export (all-defined-out))
(export (all-from-out racket))
(export (except-out racket first rest))
(export (prefix-out guilde: racket/base))

(module m racket
  (provide counter increment!)
  (define counter 0)
  (define (increment!)
    (set! counter (+ 1 counter))))

(require 'm)

counter
(increment!)
counter
(set! counter 2)

(module n racket
  (provide (contract-out [amount (and/c number? positive?)]))
  (define amount 0))
; contract will be check on boundary, module is the boundary here
(require 'n)

(module+ o
  (require (submod ".." n))
  (+ amount 10)
  )

; create a boundary between the redex & continuation
(define/contract amount
  (and/c number? positive?)
  0)

(+ 1 1)

amount

(define/contract (deposit amount)
  (-> number? any)
  (+ amount 10))

(deposit 'a)

(define/contract (hi name)
  (-> any/c any/c)
  (values 'hi name))

(hi 1)

(define (amount? a)
  (and (number? a) (integer? a) (exact? a) (>= a 0)))

(define/contract (balance)
  (-> amount?)
  1.0)

(balance)

; TODO: constructs : a thorough example
; TODO: other constructs
; TODO: Input and Output
; TODO: Regular Expressions




