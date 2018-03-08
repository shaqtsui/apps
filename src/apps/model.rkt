#lang racket/base

(require db)

(struct blog (db))
(struct post (db id))

(define (initialize-blog! home)
  (define db (sqlite3-connect #:database home #:mode 'create))
  (define the-blog (blog db))
  (unless (table-exists? db "posts")
    (query-exec db
		"create table posts (id integer primary key, title text, body text)")
    (blog-insert-post! the-blog "BMW 5" "My BMW 5 is F18")
    (blog-insert-post! the-blog "First Post" "Hey! this is my firest post."))
  (unless (table-exists? db "comments")
    (query-exec db
		"create table comments (pid integer, content text)")
    (post-insert-comment! the-blog (car (blog-posts the-blog)) "good car!")
    (post-insert-comment! the-blog (car (blog-posts the-blog)) "Agree!"))
  the-blog)



(define (blog-insert-post! a-blog title body)
  (query-exec
   (blog-db a-blog)
   "insert into posts (title, body) values (?, ?)"
   title
   body))

(define (post-insert-comment! a-blog a-post a-comment)
  (query-exec
   (blog-db a-blog)
   "insert into comments (pid, content) values (?, ?)"
   (post-id a-post)
   a-comment))

(define (blog-posts a-blog)
  (map (lambda (id) (post (blog-db a-blog) id))
       (query-list
	(blog-db a-blog)
	"select id from posts")))

(define (post-title a-post)
  (query-value
   (post-db a-post)
   "select title from posts where id = ?"
   (post-id a-post)))

(define (post-body a-post)
  (query-value
   (post-db a-post)
   "select body from posts where id = ?"
   (post-id a-post)))

(define (post-comments a-post)
  (query-list
   (post-db a-post)
   "select content from comments where pid = ?"
   (post-id a-post)))


(provide blog? blog-posts
	 post? post-title post-body post-comments
	 initialize-blog!
	 blog-insert-post! post-insert-comment!)
