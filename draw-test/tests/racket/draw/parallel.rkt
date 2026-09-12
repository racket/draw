#lang racket/base
(require racket/class
         racket/draw
         racket/port
         racket/file)

;; Try writing and reading in a parallel thread

(for ([fmt (in-list '(jpeg png))])
  (define fn (make-temporary-file))
  (define bitmap (make-object bitmap% 10 10))  
  (define worker
    (thread
     (lambda ()
       (send bitmap save-file
             fn
             'jpeg))
     #:pool 'own))  
  (thread-wait worker)
  (define bitmap2 #f)
  (define worker2
    (thread
     (lambda ()
       (set! bitmap2 (read-bitmap fn)))
     #:pool 'own))
  (thread-wait worker2)
  (delete-file fn))
