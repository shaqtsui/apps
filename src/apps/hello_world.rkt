#lang slideshow

(circle 10)

(rectangle 10 30)

(hc-append (circle 10) (rectangle 10 30))

(define c (circle 10))

(define r (rectangle 10 30))

c

r

(hc-append c r)

(define (square n)
					; comments
  (filled-rectangle n n)
  )

(square 10)

(define (four p)
  (define two-p (hc-append p p))
  (vc-append two-p two-p))

(four c)

(four r)

(define (checker p1 p2)
  (let ([p12 (hc-append p1 p2)]
	[p21 (hc-append p2 p1)])
    (vc-append p12 p21)))

(checker (colorize r "red") (colorize r "black"))

(checker (colorize (square 10) "red") (colorize (square 10) "black"))

(define (checkerboard p)
  (let* ([rp (colorize p "red")]
	 [bp (colorize p "black")]
	 [c (checker rp bp)]
	 [c4 (four c)])
    (four c4)))

(checkerboard (square 10))

circle

(define (series mk)
  ; 10 is the gap
  (hc-append 10 (mk 5) (mk 10) (mk 20)))


(series circle)

(series (lambda (size) (checkerboard (square size))))


(define series
  (lambda (mk)
    (hc-append 5 (mk 5) (mk 10) (mk 20))))

; lexical scope, mk in lambda refer to mk in argument

(define (rgb-series mk)
  (vc-append (series (lambda (sz) (colorize (mk sz) "red")))
	     (series (lambda (sz) (colorize (mk sz) "green")))
	     (series (lambda (sz) (colorize (mk sz) "blue"))))
  )

(rgb-series circle)
(rgb-series square)

(define (rgb-maker mk)
  (lambda (size)
    (vc-append (colorize (mk size) "red")
	       (colorize (mk size) "green")
	       (colorize (mk size) "blue"))))


(series (rgb-maker circle))

(series (rgb-maker square))






					; list


(list "red" "green" "blue")

(list (circle 10) (square 10))

(define (rainbow p)
  (map (lambda (color)
	 (colorize p color))
       (list "red" "orange" "yellow" "green" "blue" "purple")))

(rainbow (square 10))

(apply vc-append (rainbow (square 10)))







					; modules
(filled-flash 40 30)

; require module in collection
(require pict/flash);module implmented in flash.rkt located in pict collection, if no slash, refer to main.rkt

					; some collection of modules distributed as packages can installed via raco
					;e.g. raco setup avl




(filled-flash 40 30)
; require relative module
(require "quick.rkt")


(require slideshow/code)

; code is not function but a syntactic form
(code (circle 10))
					; libraries can export syntatic form

					; so racket is structuring a language


					; create new syntatic form (macro). syntactic extension(create a new language)

(define-syntax pict+code
  (syntax-rules ()
    [(pict+code expr);a pattern for use of the macro
     (hc-append 10
		expr
		(code expr))]))

(pict+code (circle 10)) ; instance of pattern will be replaced by instance of the corresponding template

					; syntactic extension cuts both ways: easier to say what you want, harder for other to understand






					; objects

					; class names end with %
(require racket/class
	 racket/gui/base)

(define f (new frame% [label "my art"]
	       [width 300]
	       [height 300]
	       [alignment '(center center)]))


(send f show #t) ; call f's show with argument #t


(define (add-drawing p)
  (let ([drawer (make-pict-drawer p)])
    (new canvas% [parent f]
	 [style '(border)]
	 [paint-callback (lambda (self dc)
			   (drawer dc 0 0))])))

(add-drawing (pict+code (circle 10)))

(add-drawing (colorize (filled-flash 50 30) "yellow"))

(add-drawing (square 50))


; racket is not a 'minimalist' language(scheme)
