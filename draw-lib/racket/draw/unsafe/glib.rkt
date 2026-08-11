#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/vm
	 ffi/vcruntime
         ffi/unsafe/runtime-lib)

(provide (protect-out
          define-glib
          define-gmodule
          define-gobj))

(define-runtime-lib glib-lib
  [(and macosx 64)
   (so "libintl.8.dylib")
   (so "libglib-2.0.0.dylib")]
  [macosx
   (so "libintl.9.dylib")
   (so "libglib-2.0.0.dylib")]
  [(and windows 64)
   (so "iconv-2.dll")
   (so "intl-8.dll")
   (so "pcre2-8.dll")
   (so "glib-2.0-0.dll")]
  [windows
   (so "libiconv-2.dll")
   (so "libintl-9.dll")
   (so "libglib-2.0-0.dll")]
  [else (ffi-lib "libglib-2.0" '("0" ""))])

(define-runtime-lib gmodule-lib
  [macosx
   (so "libgthread-2.0.0.dylib")
   (so "libgmodule-2.0.0.dylib")]
  [(and windows 64)
   (so "gmodule-2.0-0.dll")]
  [windows
   (so "libgthread-2.0-0.dll")
   (so "libgmodule-2.0-0.dll")]
  [else (ffi-lib "libgmodule-2.0" '("0" ""))])

(define-runtime-lib libffi-lib
  ;; needed by libgobject
  [(and macosx 64)
   (so "libffi.8.dylib")]
  [macosx
   (so "libffi.6.dylib")]
  [(and windows 64)
   (so "ffi-8.dll")]
  [windows
   (so "libffi-6.dll")]
  [else
   ;; If an expected version is not available, then assume it's not
   ;; natipkg, and shared-library search when libgobject is loaded
   (ffi-lib "libffi" '("6" "7" "8" "") #:fail (lambda () #f))])

(define-runtime-lib gobj-lib
  [macosx
   (so "libgobject-2.0.0.dylib")]
  [(and windows 64)
   (so "gobject-2.0-0.dll")]
  [windows
   (so "libgobject-2.0-0.dll")]
  [else (ffi-lib "libgobject-2.0" '("0" ""))])

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
