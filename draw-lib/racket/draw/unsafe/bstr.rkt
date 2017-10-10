#lang racket/base
(require ffi/unsafe)

(provide (protect-out scheme_make_sized_byte_string))

(define (scheme_make_sized_byte_string ptr len copy?)
  (cond
    [copy?
     (define bstr (make-bytes len))
     (memcpy bstr ptr len)
     bstr]
    [else (make-sized-byte-string ptr len)]))
