#lang racket

(require xml net/url racket/control)

(define dispatch-table (make-hash))

(hash-set! dispatch-table "hello" (lambda (query)
				    `(html (body "Hello, World!"))))

(define (dispatch str-path)
  (define url (string->url str-path))
  (define path (map path/param-path (url-path url)))
  (define h (hash-ref dispatch-table (car path) #f))
  (if h
      (prompt (h (url-query url)))
      `(html (head (title "Error"))
	     (body "No handler found for: " ,(car path)))))


(define (serve port-no)
  (define listener (tcp-listen port-no 5 #t))

  
  (define (accept-and-handle)
    (define cust (make-custodian))
    (custodian-limit-memory cust (* 50 1024 1024))
    (parameterize ([current-custodian cust])
      (define-values (in out) (tcp-accept listener))
      (thread (lambda ()
		#;(sleep (random 10))
		(define req
		  (regexp-match #rx"^GET (.+) HTTP/[0-9]+\\.[0-9]+"
				(read-line in)))
		(regexp-match #rx"(\r\n|^)\r\n" in)
		(let ([xexpr (dispatch (list-ref req 1))])
		  (display "HTTP/1.0 200 Okay\r\n" out)
		  (display "Server: k\r\n" out)
		  (display "Content-Type: text/html\r\n\r\n" out)
		  (display (xexpr->string xexpr) out))
		(close-input-port in)
		(close-output-port out))))
    
    (thread (lambda ()
	      (sleep 10)
	      (custodian-shutdown-all cust)))
    
    (accept-and-handle))

  
  (define t (thread accept-and-handle))
  (lambda ()
    (kill-thread t)
    (tcp-close listener)))


(define (build-page label next-url hidden)
  `(html (head (title "enter a number to add"))
	 (body (form ([action ,next-url]
		      [method "get"])
		     ,label
		     (input ([type "text"]
			     [name "number"]
			     [value ""]))
		     (input ([type "hidden"]
			     [name "hidden"]
			     [value ,hidden]))
		     (input ([type "submit"]
			     [name "enter"]
			     [value "Enter"]))))))


(define (many query)
  (build-page "number of greetings: "
	      "/reply"
	      ""))

(define (reply query)
  (define n (string->number (cdr (assq 'number query))))
  `(html (body ,@(for/list ([i (in-range n)])
		   " Hello"))))


(hash-set! dispatch-table "many" many)
(hash-set! dispatch-table "reply" reply)


(define (sum query)
  (build-page "First Number: " "/one" ""))

(define (one query)
  (build-page "Second Number: " "/two" (cdr (assq 'number query))))

(define (two query)
  (let ([n (string->number (cdr (assq 'hidden query)))]
	[m (string->number (cdr (assq 'number query)))])
    `(html (body "The sum is: "
		 ,(number->string (+ m n))))))

(hash-set! dispatch-table "sum" sum)
(hash-set! dispatch-table "one" one)
(hash-set! dispatch-table "two" two)


(define (sum2 query)
  (define (get-number label)
    (define query (let/cc k
			  (define tag (format "k~a" (current-inexact-milliseconds)))
			  (hash-set! dispatch-table tag k)
			  (abort (build-page label (string-append "/" tag) ""))))
    (string->number (cdr (assq 'number query))))
  (define m (get-number "First Number for sum2 *: "))
  (define n (get-number "Second Number for sum2: "))
  `(html (body "The sum for sum2 is: "
	       ,(number->string (+ m n)))))


(hash-set! dispatch-table "sum2" sum2)
