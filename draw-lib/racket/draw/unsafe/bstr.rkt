#lang racket/base
(require ffi/unsafe)

(provide (protect-out scheme_make_sized_byte_string))

(define (scheme_make_sized_byte_string ptr len copy)
  (cond
    [(positive? copy)
     (define actual-len (if (= len -1)
                            (if ptr
                                (let loop ([i 0])
                                  (cond
                                    [(zero? (ptr-ref ptr _byte i)) i]
                                    [else (loop (add1 i))]))
                                0)
                            len))
     (define bstr (make-bytes actual-len))
     (memcpy bstr ptr actual-len)
     bstr]
    [else (make-sized-byte-string ptr len)]))
