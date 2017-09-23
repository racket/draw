#lang racket/base
(require ffi/unsafe
         "../unsafe/cairo.rkt")

(provide make-port-writer
         port-writer-wait)

(define (make-port-writer port) 
  (let ([t (thread/suspend-to-kill
            (lambda ()
              (let loop ()
                (let ([msg (thread-receive)])
                  (when (bytes? msg)
                    (write-bytes msg port)
                    (loop))))))])
    (values t
            (lambda (bytes len)
              (define bstr (make-bytes len))
              (memcpy bstr bytes len)
              (thread-send t bstr void)
              CAIRO_STATUS_SUCCESS))))

(define (port-writer-wait t)
  (thread-resume t)
  (thread-send t eof void)
  (thread-wait t))

