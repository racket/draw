#lang racket/base
(require ffi/unsafe
         racket/port)

(provide callback-atomic?

         sanitize-input-port
         sanitize-output-port
         flush-sanitized-output)

;; The Racket BC can handle concurrent callbacks in different Racket
;; threads, because it copies the C stack in and out to implement
;; threads. The Racket CS cannot do that, so callbacks have to be
;; atomic. At the same time, we need some atomic callbacks to be able
;; to escape with an exception.

(define callback-atomic? (eq? 'chez-scheme (system-type 'vm)))

;; Atomicity implies that a callback cannot read from or write to an
;; arbitrary port, so we have to "sanitize" a port by adding an
;; intermediary. Unfortunately, this means that reading from a port
;; has to be eager.

(define output-ports (make-weak-hasheq))

(define (sanitize-input-port i)
  (cond
    [callback-atomic?
     (define-values (p-in p-out) (make-pipe))
     (copy-port i p-out)
     (close-output-port p-out)
     p-in]
    [else i]))
  
(define (sanitize-output-port o #:key [key o])
  (cond
    [callback-atomic?
     (define-values (p-in p-out) (make-pipe))
     (hash-set! output-ports key
                (make-ephemeron key
                                (lambda ()
                                  (close-output-port p-out)
                                  (copy-port p-in o))))
     p-out]
    [else o]))
  
(define (flush-sanitized-output key)
  (define e (hash-ref output-ports key #f))
  (define thunk (and e (ephemeron-value e)))
  (when thunk (thunk)))
