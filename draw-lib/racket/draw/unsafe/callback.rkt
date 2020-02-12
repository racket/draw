#lang racket/base
(require ffi/unsafe
         ffi/unsafe/atomic
         ffi/unsafe/vm
         racket/port)

;; The 'racket VM can handle concurrent callbacks in different Racket
;; threads, because it copies the C stack in and out to implement
;; threads. The 'chez-scheme VM cannot do that, so callbacks have to
;; be atomic. Additional work is needed to allow erroe escapes; we
;; assume that any error happens in an atomic callback that was
;; triggered by a foreign call that disables interrupts (which seems
;; like a fragile assumption!).
;;
;; Atomicity implies that a callback cannot read from or write to an
;; arbitrary port, so we have to "sanitize" a port by adding an
;; intermediary. Unfortunately, this means that reading from a port
;; has to be eager.

(provide callback-atomic?
         guard-foreign-escape

         sanitize-input-port
         sanitize-output-port
         flush-sanitized-output)

(define callback-atomic? (eq? 'chez-scheme (system-type 'vm)))

(define-syntax-rule (guard-foreign-escape e0 e ...)
  (call-guarding-foreign-escape (lambda () e0 e ...)))

(define (call-guarding-foreign-escape thunk)
  (if callback-atomic?
      ((call-with-c-return
        (lambda ()
          (with-handlers ([(lambda (x) #t)
                           (lambda (x)
                             ;; Deliver an exception re-raise after returning back
                             ;; from `call-with-c-return`:
                             (lambda ()
                               (end-atomic) ; error happened during atomic mode
                               (enable-interrupts) ; ... with interrupts disabled
                               (void/reference-sink call-with-c-return-box)
                               (raise x)))])
            (let ([vs (call-with-values thunk list)])
              ;; Deliver successful values after returning back from
              ;; `call-with-c-return`:
              (lambda ()
                (void/reference-sink call-with-c-return-box)
                (apply values vs)))))))
      (thunk)))

(define call-with-c-return-box (box #f))

;; `call-with-c-return` looks like a foreign function, due to a cast
;; to and from a callback, so returning from `call-with-c-return` will
;; pop and C frame stacks (via longjmp internally) that were pushed
;; since `call-with-c-return` was called.
(define call-with-c-return
  (and callback-atomic?
       (cast (lambda (thunk) (thunk))
             (_fun #:atomic? #t
                   #:keep call-with-c-return-box
                   _racket -> _racket)
             (_fun _racket -> _racket))))

(define enable-interrupts
  (and callback-atomic?
       (vm-primitive 'enable-interrupts)))

;; ----------------------------------------

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
