#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/vm
         "../private/libs.rkt")

(provide (protect-out
          define-glib
          define-gmodule
          define-gobj))

(define-runtime-lib glib-lib
  [(unix) (ffi-lib "libglib-2.0" '("0" ""))]
  [(macosx)
   (ffi-lib "libintl.9.dylib")
   (ffi-lib "libglib-2.0.0.dylib")]
  [(windows)
   (ffi-lib "libiconv-2.dll")
   (ffi-lib "libintl-9.dll")
   (ffi-lib "libglib-2.0-0.dll")])

(define-runtime-lib gmodule-lib
  [(unix) (ffi-lib "libgmodule-2.0" '("0" ""))]
  [(macosx)
   (ffi-lib "libgthread-2.0.0.dylib")
   (ffi-lib "libgmodule-2.0.0.dylib")]
  [(windows)
   (ffi-lib "libgthread-2.0-0.dll")
   (ffi-lib "libgmodule-2.0-0.dll")])

(define-runtime-lib gobj-lib
  [(unix) (ffi-lib "libgobject-2.0" '("0" ""))]
  [(macosx)
   (ffi-lib "libffi.6.dylib")
   (ffi-lib "libgobject-2.0.0.dylib")]
  [(windows) 
   (ffi-lib "libffi-6.dll")
   (ffi-lib "libgobject-2.0-0.dll")])

(define-ffi-definer define-glib glib-lib)
(define-ffi-definer define-gmodule gmodule-lib)
(define-ffi-definer define-gobj gobj-lib)

;; Route glib logging to Racket logging:
(define-glib g_log_set_default_handler (_fun _fpointer _pointer -> _fpointer))
(case (system-type 'vm)
  [(racket)
   (define f (get-ffi-obj 'scheme_glib_log_message #f _fpointer (lambda () #f)))
   (when f
     (void (g_log_set_default_handler f #f)))]
  [(chez-scheme)
   (define f (vm-primitive 'glib-log-message))
   (void (g_log_set_default_handler (cast f _uintptr _pointer) #f))])
