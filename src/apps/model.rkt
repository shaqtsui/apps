#lang racket/base

(struct blog (home posts) #:mutable #:prefab)
  
(struct post (title body comments) #:mutable #:prefab)
					; create constructor, predicate, accessor,

(define p1 (post "BMW 5" "My BMW 5 is F18" '("good car!" "Agree")))
(define p2 (post "First Post" "Hey! this is my firest post." '()))

(post? p1)

(define (initialize-blog! home)
  (define (missing-exn-handler exn)
    (println exn)
    (blog home
	  (list p1 p2)))
  (define the-blog
    (with-handlers ([exn? missing-exn-handler])
      (with-input-from-file home read)))
  (set-blog-home! the-blog home) ; update home if the file moved manually
  the-blog)


(define (save-blog! a-blog)
  (define (write-to-blog)
    (write a-blog))
  (with-output-to-file (blog-home a-blog)
    write-to-blog
    #:exists 'replace))



(define (blog-insert-post! a-blog title body)
    (set-blog-posts! a-blog (cons (post title body (list))
				  (blog-posts a-blog)))
    (save-blog! a-blog))

(define (post-insert-comment! a-blog a-post a-comment)
    (set-post-comments! a-post (append  
				(post-comments a-post)
				(list a-comment)))
    (save-blog! a-blog))
#|
(define BLOG
  (initialize-blog! "the-blog-data.db"))
BLOG
(blog-home BLOG)
(post-title (car (blog-posts BLOG)))
(blog-insert-post! BLOG "3rd post" "this is the 3rd post")
(save-blog! BLOG )
|#

(provide blog? blog-posts
	 post? post-title post-body post-comments
	 initialize-blog!
	 blog-insert-post! post-insert-comment!)
