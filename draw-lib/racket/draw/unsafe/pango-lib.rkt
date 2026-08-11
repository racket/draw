#lang racket/base
(require ffi/unsafe
	 ffi/unsafe/runtime-lib)

(provide (protect-out pango-lib
                      pangowin32-lib
                      pangocairo-lib))

(define-runtime-lib pango-lib
  [macosx
   (so "libfribidi.0.dylib")
   (so "libpango-1.0.0.dylib")]
  [(and windows 64)
   (so "gio-2.0-0.dll")
   (so "fribidi-0.dll")
   (so "harfbuzz.dll")
   (so "pango-1.0-0.dll")]
  [windows
   (so "libfribidi-0.dll")
   (so "libpango-1.0-0.dll")]
  [else (ffi-lib "libpango-1.0" '("0" ""))])

(define-runtime-lib pangowin32-lib
  [macosx]
  [(and windows 64)
   (so "pangowin32-1.0-0.dll")]
  [windows
   (so "libpangowin32-1.0-0.dll")]
  [else #f])

(define-runtime-lib pangocairo-lib
  [macosx
   (so "libharfbuzz.0.dylib")
   (so "libpangoft2-1.0.0.dylib")
   (so "libpangocairo-1.0.0.dylib")]
  [(and windows 64)
   (so "pangoft2-1.0-0.dll")
   (so "pangocairo-1.0-0.dll")]
  [windows
   (so "libharfbuzz-0.dll")
   (so "libpangoft2-1.0-0.dll")
   (so "libpangocairo-1.0-0.dll")]
  [else (ffi-lib "libpangocairo-1.0" '("0" ""))])
