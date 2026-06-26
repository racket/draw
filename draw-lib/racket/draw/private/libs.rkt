#lang racket/base
(require ffi/unsafe/runtime-lib
         ffi/winapi
         (for-syntax racket/base
                     syntax/parse/pre
                     ffi/winapi))

(provide (rename-out [define-runtime-lib/legacy
                       define-runtime-lib])
         win64?
         (for-syntax win64?))

(begin-for-syntax
  (define-syntax-class :system-spec
    #:attributes (modern)
    #:datum-literals (windows win32 win64 macosx)
    ;; legacy cases --------------------
    (pattern (windows)
             #:attr modern #'windows)
    (pattern (win32)
             #:attr modern #'(and windows 32))
    (pattern (win64)
             #:attr modern #'(and windows 64))
    (pattern (macosx)
             #:attr modern #'macosx)
    ;; modern cases --------------------
    (pattern any
             #:attr modern #'any)))

(define-syntax (define-runtime-lib/legacy stx)
  (syntax-parse stx
    #:literals (else)
    #:datum-literals (unix)
    [(_ lib-id:id
        ;; old-style "else", support legacy `system` specs
        [(unix) unix-lib-expr ...+]
        [system::system-spec lib ...]
        ...)
     #'(define-runtime-lib lib-id
         [system.modern lib ...]
         ...
         [else unix-lib-expr ...])]
    [(_ f ...)
     ;; assume modern syntax
     (syntax/loc stx
       (define-runtime-lib f ...))]))
