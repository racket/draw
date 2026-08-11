#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/alloc
	 ffi/unsafe/runtime-lib
	 ffi/vcruntime
         setup/dirs
         "../private/utils.rkt")

(define-runtime-lib fontconfig-lib
  [(and macosx 64)
   (so "libpng16.16.dylib")
   (so "libexpat.1.dylib")
   (so "libfreetype.6.dylib")
   (so "libfontconfig.1.dylib")]
  [macosx
   (so "libpng16.16.dylib")
   (so "libexpat.1.dylib")
   (so "libuuid.1.dylib")
   (so "libfreetype.6.dylib")
   (so "libfontconfig.1.dylib")]
  [(and windows 64)
   (so "zlib1.dll")
   (so "bz2.dll")
   (so "libpng16.dll")
   (so "brotlicommon.dll")
   (so "brotlidec.dll")
   (so "freetype.dll")
   (so "libexpat.dll")
   (so "fontconfig-1.dll")]
  [windows
   (so "zlib1.dll")
   (so "libiconv-2.dll")
   (so "libintl-9.dll")
   (so "libpng16-16.dll")
   (so "libexpat-1.dll")
   (so "libfreetype-6.dll")
   (so "libfontconfig-1.dll")]
  [else (ffi-lib "libfontconfig" '("1" ""))])

(define-runtime-lib cairo-lib
  [macosx
   (so "libpixman-1.0.dylib")
   (so "libcairo.2.dylib")]
  [(and windows 64)
   (so "pixman-1-0.dll")
   (so "cairo-2.dll")]
  [windows
   (so "libpixman-1-0.dll")
   (so "libcairo-2.dll")]
  [else (ffi-lib "libcairo" '("2" ""))])

;; A Racket-specific patch to Fontconfig defines FcSetFallbackDirs(),
;; which lets us set default paths to point to a Racket-specific
;; directory. If FcSetFallbackDirs() isn't defined, then we want
;; the system-defined directories, anyway.
(let ([FcSetFallbackDirs (get-ffi-obj 'FcSetFallbackDirs
                                      fontconfig-lib
                                      (_fun _path _path -> _void)
                                      (lambda () #f))]
      [FcSetConfigDir (get-ffi-obj 'FcSetConfigDir
                                   fontconfig-lib
                                      (_fun _path -> _void)
                                      (lambda () #f))])
  (when (and FcSetFallbackDirs
             FcSetConfigDir)
    (define share-dir (find-share-dir))
    (when share-dir
      (FcSetFallbackDirs (build-path share-dir "fonts")
                         (build-path (find-system-path 'addon-dir) "font-cache"))
      (FcSetConfigDir (build-path share-dir "fonts")))))

(provide (protect-out cairo-lib))
