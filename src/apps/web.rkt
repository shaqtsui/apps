#lang racket
(require "model.rkt")


(define (render-greeting a-name)
  (response/xexpr
   `(html
     (head (title "Welcom"))
     (body (p ,(string-append "Hello " a-name))))))



(define (render-post a-blog a-post embed/url)
  `(div ((class "post"))
	(a ((href ,(embed/url (lambda (request) (render-post-detail-page a-blog a-post request)))))
	   ,(post-title a-post))
	(p ,(post-body a-post))
	(p ,(number->string (length (post-comments a-post))) " comment(s)")))

(define (render-posts a-blog embed/url)
  `(div ((class "posts"))
	,@(map (lambda (a-post) (render-post a-blog a-post embed/url)) (blog-posts a-blog))))


(define (can-parse-post? bindings)
  (and (exists-binding? 'title bindings)
       (exists-binding? 'body bindings)))

#;
(define (start request)
  (define a-blog
    (cond [(can-parse-post? (request-bindings request))
	   (cons (parse-post (request-bindings request))
		 BLOG)]
	  [else
	   BLOG]))
  (render-blog-page a-blog request))



  (define (phase-1 request)
    (define (response-generator embed/url)
      (response/xexpr
       `(html
	 (body (h1 "phase 1")
	       (a ((href ,(embed/url phase-2)))
		  "click me")))))
    (send/suspend/dispatch response-generator))

  (define (phase-2 request)
    (define (response-generator embed/url)
      (response/xexpr
       `(html
	 (body (h1 "phase 2")
	       (a ((href ,(embed/url phase-1)))
		  "click me")))))
    (send/suspend/dispatch response-generator))


  #;
  (define (start request)
  (phase-1 request))


  (define (show-counter n request)
    (define (response-generator embed/url)
      (response/xexpr
       `(html
	 (head (title "Counting example")
	       (body (a ((href ,(embed/url show-counter-wrapper)))
			,(number->string n)))))))
    (define (show-counter-wrapper request)
      (show-counter (+ n 1) request))
    (send/suspend/dispatch response-generator))


  #;
  (define (start request)
  (show-counter 0 request))



  (define (render-blog-page a-blog request)
    (define (response-generator embed/url)
      (response/xexpr
       `(html (head (title "my blog")
		    (link ((rel "stylesheet")
			   (href "/web-static.css")
			   (type "text/css"))))
	      (body (h1 "my blog")
		    ,(render-posts a-blog embed/url)
		    (form ((action ,(embed/url render-blog-page-wrapper)))
			  (input ((name "title")))
			  (input ((name "body")))
			  (input ((type "submit"))))))))
    
    (define (render-blog-page-wrapper request)
      (blog-insert-post! a-blog
			 (extract-binding/single 'title (request-bindings request))
			 (extract-binding/single 'body (request-bindings request)))
      (render-blog-page a-blog
			(redirect/get)))
    (send/suspend/dispatch response-generator))

  (define (render-post-detail-page a-blog a-post request)
    (define (response-generator embed/url)
      (response/xexpr
       `(html (head (title "Post Details"))
	      (body (h1 "Post Details")
		    (a ((href ,(embed/url (lambda (request)
					    (render-blog-page a-blog request)))))
		       "back")
		    (h2 ,(post-title a-post))
		    (p ,(post-body a-post))
		    (ul ,@(map (lambda (comment) (list 'li comment)) 
			       (post-comments a-post)))
		    (form ((action ,(embed/url (lambda (request)
						 (render-confirm-add-comment-page 
						  a-blog
						  a-post
						  (extract-binding/single 'comment (request-bindings request))
						  request)))))
			  (input ((name "comment")))
			  (input ((type "submit"))))))))

    (send/suspend/dispatch response-generator))


  (define (render-confirm-add-comment-page a-blog a-post a-comment request)
    (define (response-generator embed/url)
      (response/xexpr
       `(html (head (titil "Add a comment"))
	      (body (h1 "Add a Comment")
		    "The comment: "
		    (div (p ,a-comment))
		    " will be added to "
		    (div ,(post-title a-post))
		    (p (a ((href ,(embed/url render-post-detail-page-wrapper-confirm)))
			  "Yes, add it"))
		    (p (a ((href ,(embed/url render-post-detail-page-wrapper-cancel)))
			  "No, I changed my mind"))))))
    (define (render-post-detail-page-wrapper-confirm request)
      (post-insert-comment! a-blog a-post a-comment)
      (render-post-detail-page a-blog a-post (redirect/get)))
    (define (render-post-detail-page-wrapper-cancel request)
      (render-post-detail-page a-blog a-post request))
    (send/suspend/dispatch response-generator))

  (define (start request)
    (render-blog-page  (initialize-blog!
			"/home/shark/blog-data.db")
		       request))





  #;(static-files-path "racket-static")



  (require web-server/servlet)

  (provide/contract (start (request? . -> . response?)))

  (require web-server/servlet-env)

  (serve/servlet start
		 #:launch-browser? #f
		 #:quit? #f
		 #:listen-ip #f
		 #:port 8000
		 #:servlet-path "/servlets/a")

  
