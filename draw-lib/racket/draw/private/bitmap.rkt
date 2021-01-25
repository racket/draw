#lang racket/base
(require racket/class
         racket/unsafe/ops
         racket/port
         (prefix-in c: racket/contract)
         file/convertible
         ffi/unsafe
         (for-syntax racket/base)
         "syntax.rkt"
         "hold.rkt"
         "../unsafe/cairo.rkt"
         "../unsafe/png.rkt"
         "../unsafe/jpeg.rkt"
         (only-in "../unsafe/callback.rkt" guard-foreign-escape)
         "../xbm.rkt"
         "../xpm.rkt"
         "../bmp.rkt"
         "../gif.rkt"
         "local.rkt"
         "color.rkt"
         "lock.rkt")

(provide bitmap%
         make-bitmap
         make-platform-bitmap
         read-bitmap
         make-monochrome-bitmap
         -bitmap-dc%
         (protect-out make-alternate-bitmap-kind
                      build-cairo-surface
                      quartz-bitmap%
                      win32-no-hwnd-bitmap%
                      install-bitmap-dc-class!
                      surface-flush)
         (c:contract-out
          [specialize-unknown-kind
           (c:-> input-port?
                 (c:or/c 'unknown 'unknown/mask 'unknown/alpha)
                 (c:or/c 'png/mask 'png/alpha 'png
                         'jpeg 'gif 'bmp/alpha
                         'bmp 'xbm 'xpm 'xbm))]))

(define -bitmap-dc% #f)
(define (install-bitmap-dc-class! v) (set! -bitmap-dc% v))

;; FIXME: there must be some way to abstract over all many of the
;; ARGB/RGBA/BGRA iterations.

(define-struct alternate-bitmap-kind (width height scale))

(define-local-member-name
  get-alphas-as-mask
  set-alphas-as-mask
  surface-flush)

(define (bitmap-file-kind-symbol? s)
  (memq s '(unknown unknown/mask unknown/alpha
                    gif gif/mask gif/alpha
                    jpeg jpeg/alpha
                    png png/mask png/alpha
                    xbm xbm/alpha
                    xpm xpm/alpha
                    bmp bmp/alpha
                    pict)))

(define (bitmap-save-kind-symbol? s)
  (memq s '(png jpeg gif xbm xpm bmp)))

(define (quality-integer? i)
  (and (exact-nonnegative-integer? i) (i . <= . 100)))

(define (destroy s)
  (cairo_surface_destroy s))

(define (argb-indices)
  (if (system-big-endian?)
      (values 0 1 2 3)
      (values 3 2 1 0)))

(define (a-index)
  (if (system-big-endian?) 0 3))

(define (b-index)
  (if (system-big-endian?) 3 0))

(define fx+ unsafe-fx+)
(define fx* unsafe-fx*)

(define mult-table #f)
(define unmult-table #f)

(define (get-mult-table)
  (atomically
   (unless mult-table
     (set! mult-table (make-bytes (* 256 256)))
     (for ([a (in-range 256)])
       (for ([v (in-range 256)])
         (bytes-set! mult-table
                     (fx+ (fx* a 256) v)
                     (unsafe-fl->fx
                      (unsafe-flround
                       (unsafe-fl/
                        (unsafe-fx->fl (fx* a v))
                        255.0))))))))
  mult-table)

(define (get-unmult-table)
  (atomically
   (unless unmult-table
     (set! unmult-table (make-bytes (* 256 256)))
     (for ([a (in-range 256)])
       (for ([v (in-range 256)])
         (bytes-set! unmult-table
                     (fx+ (fx* a 256) v)
                     (if (unsafe-fx<= a v)
                         255
                         (unsafe-fl->fx
                          (unsafe-flround
                           (unsafe-fl/
                            (unsafe-fx->fl (fx* 255 v))
                            (unsafe-fx->fl a))))))))))
  unmult-table)

(define (alpha-mult al v)
  (unsafe-fl->fx
   (unsafe-flround
    (unsafe-fl/
     (unsafe-fx->fl (fx* al v))
     255.0))))

(define (alpha-unmult al v)
  (if (zero? al)
      255
      (unsafe-fxmin 255
                    (unsafe-fl->fx
                     (unsafe-flround
                      (unsafe-fl/
                       (unsafe-fx->fl (fx* 255 v))
                       (unsafe-fx->fl al)))))))


(define png-convertible<%>
  (interface* ()
              ([prop:convertible
                (lambda (bm format default)
                  (case format
                    [(png-bytes)
                     (let ([s (open-output-bytes)])
                       (send bm save-file s 'png)
                       (get-output-bytes s))]
                    [(png@2x-bytes)
                     (if (= 2 (send bm get-backing-scale))
                         (let ([s (open-output-bytes)])
                           (send bm save-file s 'png #:unscaled? #t)
                           (get-output-bytes s))
                         default)]
                    [else default]))])))

(define (get-empty-surface)
  (cairo_image_surface_create CAIRO_FORMAT_ARGB32 1 1))

(define (*i x y) (inexact->exact (ceiling (* x y))))
(define (/i x y) (inexact->exact (ceiling (/ x y))))

(define (pointer-fill! p v n)
  (for ([i (in-range n)])
    (ptr-set! p _ubyte i v)))

(define bitmap%
  (class* object% (png-convertible<%>)

    ;; We support three kinds of bitmaps:
    ;;  * Color with alpha channel;
    ;;    when used as a mask, alpha channel is used;
    ;;    this is the sensible one that works nicely with Cairo
    ;;  * Black and white; alpha channel is opposite
    ;;    of value, so either value or alpha can be
    ;;    considered as mask;
    ;;    we have to play some tricks to keep the value and mask
    ;;    inverted, and to keep everything black & white (no gray)
    ;;  * Color without alpha channel; when used as a mask,
    ;;    value channel is used (i.e., inverted RGB average
    ;;    is used as an alpha);
    ;;    we have to play even worse tricks when this kind of bitmap
    ;;    is used as a mask

    (init-rest args)

    (define/public (surface-flush)
      (cairo_surface_flush s))
    
    (define alt? #f)
    (define width 0)
    (define height 0)
    (define b&w? #f)
    (define alpha-channel? #f)
    (define s #f)
    (define loaded-mask #f)
    (define backing-scale 1.0)
    (define shadow #f)
    (define data-from-file #f)

    (define alpha-s #f)
    (define alpha-s-up-to-date? #f)

    (super-new)

    (set!-values (alt? width height b&w? alpha-channel? s loaded-mask backing-scale data-from-file)
      (case-args
       args
       [([alternate-bitmap-kind? a])
        (values #t
                (alternate-bitmap-kind-width a)
                (alternate-bitmap-kind-height a)
                #f #t #f #f
                (alternate-bitmap-kind-scale a)
                #f)]
       [([exact-positive-integer? w]
         [exact-positive-integer? h]
         [any? [b&w? #f]]
         [any? [alpha? #f]]
         [positive-real? [scale 1.0]])
        (values
         #f
         w
         h
         (and b&w? #t)
         (and alpha? (not b&w?))
         (let ([s (cairo_image_surface_create CAIRO_FORMAT_ARGB32
                                              (max (*i scale w) 1)
                                              (max (*i scale h) 1))])
           (cairo_surface_flush s)
           (cond
            [(not (zero? (cairo_surface_status s)))
             #f]
            [(cairo_image_surface_get_data* s)
             (cond
              [b&w?
               ;; Init transparent white:
               (transparent-white! s w h)]
              [alpha?
               ;; Init transparent:
               (pointer-fill! (cairo_image_surface_get_data* s)
                              0
                              (* (cairo_image_surface_get_height s)
                                 (cairo_image_surface_get_stride s)))]
              [else
               ;; Init all white, 255 alpha:
               (pointer-fill! (cairo_image_surface_get_data* s) 255
                              (* (cairo_image_surface_get_height s)
                                 (cairo_image_surface_get_stride s)))])
             (cairo_surface_mark_dirty s)
             s]
            [else
             ;; bitmap creation failed
             #f]))
         #f
         (* 1.0 scale)
         #f)]
       [([(make-alts path-string? input-port?) filename]
         [bitmap-file-kind-symbol? [kind 'unknown]]
         [(make-or-false color%) [bg-color #f]]
         [any? [complain-on-failure? #f]]
         [positive-real? [scale 1.0]]
         [any? [save-data-from-file? #f]])
        (let-values ([(s b&w? data-from-file)
                      (do-load-bitmap filename kind bg-color complain-on-failure? save-data-from-file?)]
                     [(alpha?) (memq kind '(unknown/alpha gif/alpha jpeg/alpha
                                                          png/alpha xbm/alpha xpm/alpha
                                                          bmp/alpha))]
                     [(mask?) (memq kind '(unknown/mask gif/mask png/mask))])
          (let ([mask-bm
                 (and s
                      (not alpha?)
                      (not b&w?)
                      (let ([w (cairo_image_surface_get_width s)]
                            [h (cairo_image_surface_get_height s)]
                            [row-width (cairo_image_surface_get_stride s)]
                            [bstr (cairo_image_surface_get_data* s)]
                            [A (a-index)])
                        (begin0
                         (and mask?
                              ;; Move alpha channel to a separate mask bitmap
                              (let ([b&w? (for*/and ([j (in-range h)]
                                                     [i (in-range w)])
                                            (let ([v (ptr-ref bstr _ubyte (+ A (* 4 i) (* j row-width)))])
                                              (or (= v 0) (= v 255))))])
                                (let ([mask-bm (make-object bitmap% w h b&w?)])
                                  (send mask-bm set-alphas-as-mask 0 0 w h bstr row-width A w h)
                                  mask-bm)))
                         ;; Force all alpha values to 255
                         (for* ([j (in-range h)]
                                [i (in-range w)])
                           (ptr-set! bstr _ubyte (+ A (* 4 i) (* j row-width)) 255))
                         (cairo_surface_mark_dirty s))))])
            (if s
                (values #f
                        (/i (cairo_image_surface_get_width s) scale)
                        (/i (cairo_image_surface_get_height s) scale)
                        b&w?
                        (and alpha? (not b&w?))
                        s
                        mask-bm
                        (* 1.0 scale)
                        data-from-file)
                (values #f 0 0 #f #f #f #f (* 1.0 scale)
                        data-from-file))))]
       [([bytes? bstr]
         [exact-positive-integer? w]
         [exact-positive-integer? h])
        (let ([bw (quotient (+ w 7) 8)])
          (unless ((bytes-length bstr) . >= . (* h bw))
            (error (init-name 'bitmap%)
                   "given byte string is too small for dimensions: ~s"
                   bstr))
          (let ([s (cairo_image_surface_create CAIRO_FORMAT_ARGB32 w h)])
            (let ([rows (list->vector
                         (for/list ([i (in-range h)])
                           (let ([s (* i bw)])
                             (subbytes bstr s (+ s bw)))))])
              (install-bytes-rows s w h rows #t #f #f #t))
            (values #f w h #t #f s #f 1.0 #f)))]
       (init-name 'bitmap%)))

    (when (not (= backing-scale 1.0))
      (when (or b&w? loaded-mask)
        (error (init-name 'bitmap%)
               (string-append
                "~a must have a backing scale of 1.0\n"
                "  given scale: ~a")
               (if b&w?
                   "black-and-white bitmap"
                   "bitmap with mask")
               backing-scale)))

    ;; Claim memory proportional to the size of the bitmap, which
    ;; helps the GC see that we're using that much memory:
    (set! shadow (make-phantom-bytes (* width height 4)))

    ;; Use for non-alpha color bitmaps when they are used as a mask:
    (define/public (drop-alpha-s)
      (set! alpha-s-up-to-date? #f)
      (when alpha-s
        (let ([s2 alpha-s])
          (set! alpha-s #f)
          (destroy s2))))

    (def/public (get-width) (max 1 width))
    (def/public (get-height) (max 1 height))
    (def/public (get-depth) (if b&w? 1 32))
    (def/public (is-color?) (not b&w?))
    (def/public (has-alpha-channel?) (and alpha-channel? #t))
    (def/public (get-data-from-file) data-from-file)

    (define/private (check-alternate who)
      (when alt?
        (raise-mismatch-error (method-name 'bitmap% who)
                              "not available in a canvas-compatible bitmap: "
                              this)))

    (def/public (get-loaded-mask) loaded-mask)
    (def/public (set-loaded-mask [(make-or-false bitmap%) m])
      (unless (= backing-scale 1)
        (error (method-name 'bitmap% 'set-loaded-mask)
               (string-append
                "can only install a mask for a bitmap with backing scale of 1.0\n"
                "  backing scale: ~a")
               backing-scale))
      (set! loaded-mask m))

    (define/public (draw-bitmap-to cr sx sy dx dy w h alpha clipping)
      #f)

    (define/public (release-bitmap-storage)
      (drop-alpha-s)
      (when s
        (let ([s2 s])
          (set! s #f)
          (destroy s2)
          (set! shadow #f))))

    (define/public (get-bitmap-gl-context)
      #f)

    (define/public (do-self-copy dc x y w h x2 y2)
      #f)
    
    (define/public (make-dc) (make-object -bitmap-dc% this))

    (define/public (load-file in
                              [kind 'unknown]
                              [bg #f]
                              [complain-on-failure? #f]
                              #:save-data-from-file?
                              [save-data-from-file? #f])
      (check-alternate 'load-file)
      (unless (= 1 backing-scale)
        (error (method-name 'bitmap% 'load-file)
               (string-append
                "can only load a file in a bitmap with backing scale of 1.0\n"
                "  backing scale: ~a")
               backing-scale))
      (release-bitmap-storage)
      (set!-values (s b&w? data-from-file)
                   (do-load-bitmap in kind bg complain-on-failure? save-data-from-file?))
      (set! width (if s (cairo_image_surface_get_width s) 0))
      (set! height (if s (cairo_image_surface_get_height s) 0))
      (set! shadow (make-phantom-bytes (* width height 4)))
      (and s #t))

    (define/private (do-load-bitmap in kind bg complain-on-failure? save-data-from-file?)
      (cond
        [(path-string? in)
         (with-handlers ([exn:fail? (lambda (exn)
                                      (if complain-on-failure?
                                          (raise exn)
                                          (values #f #f #f)))])
           (call-with-input-file*
               in
             (lambda (in) (do-load-bitmap/port in kind bg save-data-from-file?))))]
        [else
         (do-load-bitmap/port in kind bg save-data-from-file?)]))

    (define/private (do-load-bitmap/port in kind bg save-data-from-file?)
      (cond
        [save-data-from-file?
         (define bp (open-output-bytes))
         (define-values (pipe-in pipe-out) (make-pipe))
         (define thd
           (thread (λ ()
                     (with-handlers ([exn:fail? void])
                       (copy-port in pipe-out bp))
                     (close-output-port pipe-out))))
         (define-values (s b&w) (do-load-bitmap/dispatch pipe-in kind bg))
         (thread-wait thd)
         (values s b&w (vector-immutable kind
                                         (if (and bg (not (send bg is-immutable?)))
                                             (make-color (send bg red)
                                                         (send bg green)
                                                         (send bg blue))
                                             bg)
                                         (bytes->immutable-bytes (get-output-bytes bp))))]
        [else
         (define-values (s b&w) (do-load-bitmap/dispatch in kind bg))
         (values s b&w #f)]))

    (define/private (do-load-bitmap/dispatch in kind bg)
      (case kind
        [(unknown unknown/mask unknown/alpha)
         (define new-kind (specialize-unknown-kind in kind))
         (do-load-bitmap/dispatch/known in new-kind bg)]
        [else
         (do-load-bitmap/dispatch/known in kind bg)]))

    (define/private (do-load-bitmap/dispatch/known in kind bg)
      (case kind
        [(unknown unknown/mask unknown/alpha)
         (error 'do-load-bitmap/dispatch/known "got unknown: ~s" kind)]
        [(png/alpha png/mask png)
         (do-load-bitmap/png in kind bg)]
        [(jpeg jpeg/alpha)
         (do-load-bitmap/jpeg in kind bg)]
        [(gif gif/mask gif/alpha)
         (do-load-bitmap/gif in kind bg)]
        [(xbm xbm/alpha)
         (do-load-bitmap/xbm in kind bg)]
        [(xpm xpm/alpha)
         (do-load-bitmap/xpm in kind bg)]
        [(bmp bmp/alpha)
         (do-load-bitmap/bmp in kind bg)]
        [else (values #f #f)]))

    (define/private (do-load-bitmap/png in kind bg)
      ;; Using the Cairo PNG support is about twice as fast, but we have
      ;; less control, and there are problems making deallocation reliable
      ;; (in case of exceptions or termination):
      #;
      (let ([proc (lambda (ignored bstr len)
                    (read-bytes! (scheme_make_sized_byte_string bstr len 0) in)
                    CAIRO_STATUS_SUCCESS)])
        (with-holding
            proc
          (values (cairo_image_surface_create_from_png_stream proc) #f)))
      ;; Using libpng directly:
      (let-values ([(r w h b&w? alpha?) (create-png-reader
                                         in
                                         (memq kind '(png/mask png/alpha))
                                         (and bg
                                              (list (send bg red)
                                                    (send bg green)
                                                    (send bg blue))))])
        (let ([rows (read-png r)])
          (destroy-png-reader r)
          (let* ([s (cairo_image_surface_create CAIRO_FORMAT_ARGB32 w h)]
                 [pre? (and alpha? (eq? kind 'png/alpha))])
            (install-bytes-rows s w h rows b&w? alpha? pre? #f)
            (values s b&w?)))))

    (define/private (do-load-bitmap/jpeg in kind bg)
      (let ([d (create-decompress in)])
        (guard-foreign-escape
         (dynamic-wind
          void
          (lambda ()
            (jpeg_read_header d #t)
            (jpeg_start_decompress d)
            (let ([w (jpeg_decompress_struct-output_width d)]
                  [h (jpeg_decompress_struct-output_height d)]
                  [c (jpeg_decompress_struct-output_components d)])
              (let-values ([(samps bstr) (create-jpeg-sample-array d (* w c))]
                           [(A R G B) (argb-indices)])
                (let* ([s (cairo_image_surface_create CAIRO_FORMAT_ARGB32 w h)]
                       [dest (begin
                               (cairo_surface_flush s)
                               (cairo_image_surface_get_data* s))]
                       [dest-row-width (cairo_image_surface_get_stride s)])
                  (for ([j (in-range h)])
                    (jpeg_read_scanlines d samps 1)
                    (let ([row (* dest-row-width j)])
                      (for ([i (in-range w)])
                        (let ([4i (fx+ row (fx* 4 i))]
                              [ci (fx* c i)])
                          (ptr-set! dest _ubyte (fx+ 4i A) 255)
                          (if (= c 1)
                              (let ([v (ptr-ref bstr _ubyte ci)])
                                (ptr-set! dest _ubyte (fx+ 4i R) v)
                                (ptr-set! dest _ubyte (fx+ 4i G) v)
                                (ptr-set! dest _ubyte (fx+ 4i B) v))
                              (begin
                                (ptr-set! dest _ubyte (fx+ 4i R) (ptr-ref bstr _ubyte ci))
                                (ptr-set! dest _ubyte (fx+ 4i G) (ptr-ref bstr _ubyte (fx+ ci 1)))
                                (ptr-set! dest _ubyte (fx+ 4i B) (ptr-ref bstr _ubyte (fx+ ci 2)))))))))
                  (cairo_surface_mark_dirty s)
                  (jpeg_finish_decompress d)
                  (values s #f)))))
          (lambda ()
            (destroy-decompress d))))))

    (define/private (do-load-bitmap/gif in kind bg)
      (let-values ([(w h rows) (gif->rgba-rows in)])
        (let* ([s (cairo_image_surface_create CAIRO_FORMAT_ARGB32 w h)]
               [alpha? #t]
               [pre? (and alpha? (eq? kind 'gif/alpha))]
               [b&w? #f])
          (install-bytes-rows s w h rows b&w? alpha? pre? #f)
          (values s b&w?))))

    (define/private (do-load-bitmap/xbm in kind bg)
      (let-values ([(w h rows) (read-xbm in)])
        (if rows
            (let ([s (cairo_image_surface_create CAIRO_FORMAT_ARGB32 w h)])
              (install-bytes-rows s w h rows #t #f #f #t)
              (values s #t))
            (values #f #f))))

    (define/private (do-load-bitmap/xpm in kind bg)
      (let-values ([(w h rows) (read-xpm in)])
        (if rows
            (let ([s (cairo_image_surface_create CAIRO_FORMAT_ARGB32 w h)]
                  [alpha? #t])
              (install-bytes-rows s w h rows #f alpha? #t #f)
              (values s #f))
            (values #f #f))))

    (define/private (do-load-bitmap/bmp in kind bg)
      (let-values ([(w h rows) (read-bmp in #:background (and (eq? kind 'bmp)
                                                              (if bg
                                                                  (list (color-red bg)
                                                                        (color-green bg)
                                                                        (color-blue bg))
                                                                  (list 255 255 255))))])
        (if rows
            (let ([s (cairo_image_surface_create CAIRO_FORMAT_ARGB32 w h)]
                  [alpha? #t])
              (install-bytes-rows s w h rows #f alpha? #t #f)
              (values s #f))
            (values #f #f))))

    ;; s : Cairo bitmap surface
    ;; w, h : width and height in pixels
    ;; rows : a vector of `h' byte strings
    ;; b&w? : each bit in a byte string is a pixel (so each byte
    ;;        string in `rows' is `(/ w 8)' bytes long)
    ;;        if not `backward?': low bit is first and 0 is black
    ;;        if `backward?': high bit is first and 1 is black
    ;; alpha? : relevant only if not `b&w?';
    ;;          if true: each byte string has 4 bytes per pixel, RGBA
    ;;          if false: each byte string has 3 bytes er pixel, RGB
    ;; pre? : should be #f if not `alpha?', otherwise a #t value
    ;;        means that the RGB values should be multiplied by A value
    ;;        (i.e., the values are not already pre-multiplied)
    ;; backward? : affects byte interpretation in `b&w?' mode; see above
    (define/private (install-bytes-rows s w h rows b&w? alpha? pre? backward?)
      (let* ([dest (begin
                     (cairo_surface_flush s)
                     (cairo_image_surface_get_data* s))]
             [dest-row-width (cairo_image_surface_get_stride s)]
             [m (and pre? (get-mult-table))])
        (let-values ([(A R G B) (argb-indices)])
          (for ([r (in-vector rows)]
                [j (in-naturals)])
            (let ([row (* dest-row-width j)])
              (if b&w?
                  (for ([i (in-range w)])
                    (let ([b (unsafe-fxquotient i 8)]
                          [bit (if backward?
                                   (unsafe-fxlshift 1 (unsafe-fxand i 7))
                                   (unsafe-fxrshift 128 (unsafe-fxand i 7)))]
                          [pos (fx+ row (fx* 4 i))])
                      (let* ([v (if (zero? (unsafe-fxand bit (unsafe-bytes-ref r b)))
                                    0
                                    255)]
                             [v (if backward? (- 255 v) v)])
                        (ptr-set! dest _ubyte (fx+ pos A) (- 255 v))
                        (ptr-set! dest _ubyte (fx+ pos 1) v)
                        (ptr-set! dest _ubyte (fx+ pos 2) v)
                        (ptr-set! dest _ubyte (fx+ pos B) v))))
                  (for ([i (in-range w)])
                    (let* ([4i (fx* 4 i)]
                           [pos (fx+ row 4i)]
                           [spos (if alpha?
                                     (fx* 4 i)
                                     (fx* 3 i))]
                           [al (if alpha?
                                   (unsafe-bytes-ref r (fx+ spos 3))
                                   255)]
                           [premult (lambda (al v)
                                      (if m
                                          (unsafe-bytes-ref m (fx+ (fx* al 256) v))
                                          v))])
                      (ptr-set! dest _ubyte (fx+ pos A) al)
                      (ptr-set! dest _ubyte (fx+ pos R) (premult al (unsafe-bytes-ref r spos)))
                      (ptr-set! dest _ubyte (fx+ pos G) (premult al (unsafe-bytes-ref r (fx+ spos 1))))
                      (ptr-set! dest _ubyte (fx+ pos B) (premult al (unsafe-bytes-ref r (fx+ spos 2))))))))))
        (cairo_surface_mark_dirty s)))

    (define/private (call-with-alt-bitmap x y w h sc proc)
      (let* ([bm (make-object bitmap% w h #f #t)]
             [cr (cairo_create (send bm get-cairo-surface))])
        (let ([p (cairo_get_source cr)])
          (cairo_pattern_reference p)
          (cairo_set_source_surface cr (get-cairo-surface) (- x) (- y))
          (unless (= sc 1)
            (let ([m (make-cairo_matrix_t 0.0 0.0 0.0 0.0 0.0 0.0)])
              (cairo_matrix_init_translate m 0 0)
              (cairo_matrix_scale m sc sc)
              (cairo_matrix_translate m x y)
              (cairo_pattern_set_matrix (cairo_get_source cr) m)))
          (cairo_new_path cr)
          (cairo_rectangle cr 0 0 w h)
          (cairo_fill cr)
          (cairo_set_source cr p)
          (cairo_pattern_destroy p))
        (cairo_destroy cr)
        (proc bm)
        (send bm release-bitmap-storage)))

    (define/public (save-file out [kind 'unknown] [quality 75]
                              #:unscaled? [unscaled? #f])
      (and (ok?)
           (begin
             (if (or alt?
                     (and (not unscaled?)
                          (not (= backing-scale 1))))
                 (let ([s (lambda (v)
                            (if (or (not unscaled?)
                                    (= backing-scale 1))
                                v
                                (inexact->exact (ceiling (* v backing-scale)))))])
                   (call-with-alt-bitmap
                    0 0 (s width) (s height) (if unscaled? 1 backing-scale)
                    (lambda (bm)
                      (send bm save-file out kind quality))))
                 (do-save-file out kind quality))
             #t)))

    (define/private (do-save-file out kind quality)
      (if (path-string? out)
          (call-with-output-file*
           out
           #:exists 'truncate/replace
           (lambda (out) (do-save-file out kind quality)))
          (case kind
            [(png)
             (cond
              [b&w?
               ;; Write a 1-bit png
               (let* ([b (ceiling (/ width 8))]
                      [rows (build-vector height (lambda (i) (make-bytes b)))]
                      [data (begin (surface-flush)
                                   (cairo_image_surface_get_data* s))]
                      [row-width (cairo_image_surface_get_stride s)])
                 (for ([j (in-range height)])
                   (let ([row (vector-ref rows j)])
                     (for ([bi (in-range b)])
                       (bytes-set!
                        row
                        bi
                        (let ([src (+ (* j row-width) (* (* bi 8) 4))])
                          (for/fold ([v 0]) ([k (in-range 8)])
                            (if ((+ (* 8 bi) k) . < . width)
                                (if (zero? (ptr-ref data _ubyte (+ src 3 (* 4 k))))
                                    (bitwise-ior v (unsafe-fxrshift 128 k))
                                    v)
                                v)))))))
                 (let ([w (create-png-writer out width height #t #f)])
                   (write-png w rows)
                   (destroy-png-writer w)))]
              [else #;(and (not alpha-channel?)
                           loaded-mask
                           (= width (send loaded-mask get-width))
                           (= height (send loaded-mask get-height)))
               (let* ([width (*i width backing-scale)]
                      [height (*i height backing-scale)]
                      [bstr (make-bytes (* width height 4))])
                 (get-argb-pixels 0 0 width height bstr #:unscaled? #t)
                 (when loaded-mask
                   (send loaded-mask get-argb-pixels 0 0 width height bstr #t))
                 ;; PNG wants RGBA instead of ARGB...
                 (let ([rows (build-vector height (lambda (i) (make-bytes (* 4 width))))])
                   (for ([j (in-range height)]
                         [dest-row (in-vector rows)])
                     (let ([src-row (* j (* 4 width))])
                       (for ([i (in-range width)])
                         (let* ([4i (* 4 i)]
                                [ri (+ src-row 4i)])
                           (bytes-set! dest-row 4i (bytes-ref bstr (+ 1 ri)))
                           (bytes-set! dest-row (+ 4i 1) (bytes-ref bstr (+ 2 ri)))
                           (bytes-set! dest-row (+ 4i 2) (bytes-ref bstr (+ 3 ri)))
                           (bytes-set! dest-row (+ 4i 3) (bytes-ref bstr ri))))))
                   (let ([w (create-png-writer out width height #f #t)])
                     (write-png w rows)
                     (destroy-png-writer w))))]
              #;
              [else
               ;; Use Cairo built-in support:
               (let ([proc (lambda (ignored bstr len)
                             (write-bytes (scheme_make_sized_byte_string bstr len 0) out)
                             CAIRO_STATUS_SUCCESS)])
                 (with-holding
                  proc
                  (cairo_surface_write_to_png_stream s proc)))])]
            [(jpeg)
             (let ([c (create-compress out)]
                   [width (*i width backing-scale)]
                   [height (*i height backing-scale)])
               (guard-foreign-escape
                 (dynamic-wind
                   void
                   (lambda ()
                     (set-jpeg_compress_struct-image_width! c width)
                     (set-jpeg_compress_struct-image_height! c height)
                     (set-jpeg_compress_struct-input_components! c 3)
                     (set-jpeg_compress_struct-in_color_space! c JCS_RGB)
                     (jpeg_set_defaults c)
                     (jpeg_set_quality c quality #t)
                     (jpeg_start_compress c #t)
                     (let-values ([(samps bstr) (create-jpeg-sample-array c (* width 3))]
                                  [(A R G B) (argb-indices)])
                       (cairo_surface_flush s)
                       (let* ([dest (cairo_image_surface_get_data* s)]
                              [dest-row-width (cairo_image_surface_get_stride s)]
                              [h height]
                              [w width])
                         (for ([j (in-range h)])
                           (let ([row (* dest-row-width j)])
                             (for ([i (in-range w)])
                               (let ([4i (* 4 i)]
                                     [ci (* 3 i)])
                                 (ptr-set! bstr _ubyte ci (ptr-ref dest _ubyte (+ row (+ 4i R))))
                                 (ptr-set! bstr _ubyte (+ ci 1) (ptr-ref dest _ubyte (+ row (+ 4i G))))
                                 (ptr-set! bstr _ubyte (+ ci 2) (ptr-ref dest _ubyte (+ row (+ 4i B)))))))
                           (jpeg_write_scanlines c samps 1))))
                     (jpeg_finish_compress c))
                   (lambda () (destroy-compress c)))))]
            [(bmp)
             (define bstr (make-bytes (* width height 4)))
             (get-argb-pixels 0 0 width height bstr #:unscaled? #t)
             (when loaded-mask
               (send loaded-mask get-argb-pixels 0 0 width height bstr #t))
             (define write-bmp
               (cond
                [b&w? write-bmp1]
                [(or alpha-channel? loaded-mask) write-bmp32]
                [else write-bmp24]))
             (write-bmp bstr width height out)]
            [else (error (method-name 'bitmap% 'save-file)
                         "saving not implemented for file kind: ~e"
                         kind)])))

    (def/public (ok?) (and s #t))

    (define/public (get-cairo-surface) (or s (get-empty-surface)))
    (define/public (get-cairo-target-surface) (get-cairo-surface))
    (define/public (get-cairo-alpha-surface)
      (or (if (or b&w? alpha-channel?)
              s
              (begin
                (prep-alpha (*i width backing-scale) (*i height backing-scale))
                alpha-s))
          (get-empty-surface)))

    (define/public (get-cairo-device-scale) backing-scale)

    (define/public (get-backing-scale) (get-cairo-device-scale))

    (define/public (get-handle) s)

    (define/public (get-argb-pixels x y w h bstr
                                    [get-alpha? #f]
                                    [pre-mult? #f]
                                    #:unscaled? [unscaled? #f])
      (unless ((bytes-length bstr) . >=  . (* w h 4))
        (raise-mismatch-error (method-name 'bitmap% 'get-argb-pixels)
                              "byte string is too short: "
                              bstr))
      (when (ok?)
        (unless (or (zero? w) (zero? h))
          (if (or alt?
                  (and (not unscaled?)
                       (not (= backing-scale 1))))
              (call-with-alt-bitmap
               x y w h (if unscaled? 1 backing-scale)
               (lambda (bm) (send bm get-argb-pixels 0 0 w h bstr get-alpha? pre-mult?)))
              (do-get-argb-pixels x y w h bstr get-alpha? pre-mult?
                                  (*i width backing-scale) (*i height backing-scale))))))

    (define-syntax-rule (for** ([i i-all i-hi]
                                [j j-all j-hi])
                           . body)
      ;; These combinations will cover the corner twice,
      ;; but that's ok as long as `body` is idempotent.
      ;; Anyway, we're only doing this to fill in blank areas
      ;; for a too-large request.
      (begin
        (for* ([i i-hi] [j j-all]) . body)
        (for* ([i i-all] [j j-hi]) . body)))

    (define/private (do-get-argb-pixels x y w h bstr get-alpha? pre-mult? width height)
      ;; Fill range that is beyond edge of picture:
      (if get-alpha?
          (for** ([i (in-range x (+ x w)) (in-range (max x width) (+ x w))]
                  [j (in-range y (+ y h)) (in-range (max y height) (+ y h))])
            (bytes-set! bstr (* 4 (+ (- i x) (* (- j y) w))) 255))
          (for** ([i (in-range x (+ x w)) (in-range (max x width) (+ x w))]
                  [j (in-range y (+ y h)) (in-range (max y height) (+ y h))])
            (let ([p (* 4 (+ (- i x) (* (- j y) w)))])
              (bytes-set! bstr p 255)
              (bytes-set! bstr (+ p 1) 0)
              (bytes-set! bstr (+ p 2) 0)
              (bytes-set! bstr (+ p 3) 0))))
      ;; Get pixels:
      (when (not get-alpha?)
        (let-values ([(A R G B) (argb-indices)])
          (surface-flush)
          (let ([data (cairo_image_surface_get_data* s)]
                [row-width (cairo_image_surface_get_stride s)]
                [um (and (or (and alpha-channel? (not pre-mult?)) b&w?)
                         (get-unmult-table))]
                [set-alpha? alpha-channel?])
            (let ([w2 (max 0 (min width (+ x w)))])
              (for* ([j (in-range (max 0 y) (min (+ y h) height))])
                (let* ([row (* j row-width)]
                       [p (* 4 (* (- j y) w))]
                       [ri-start (+ row (* 4 (max 0 x)))]
                       [ri-end (+ row (* 4 w2))]
                       [pi-start p]
                       [pi-end (+ p (* 4 (- w2 x)))])
                  (for ([ri (in-range ri-start ri-end 4)]
                        [pi (in-range pi-start pi-end 4)])
                    (let ([a (ptr-ref data _ubyte (+ ri A))])
                      (let-syntax ([unmult
                                    ;; Defined as a macro to copy the
                                    ;; `unsafe-bytes-ref' to each branch,
                                    ;; instead of binding a local variable
                                    (syntax-rules ()
                                      [(_ v)
                                       (if um
                                           (unsafe-bytes-ref um (fx+ (fx* a 256) v))
                                           v)])])
                        (when set-alpha?
                          (unsafe-bytes-set! bstr pi a))
                        (unsafe-bytes-set! bstr (+ pi 1) (unmult (ptr-ref data _ubyte (+ ri R))))
                        (unsafe-bytes-set! bstr (+ pi 2) (unmult (ptr-ref data _ubyte (+ ri G))))
                        (unsafe-bytes-set! bstr (+ pi 3) (unmult (ptr-ref data _ubyte (+ ri B)))))))))))))
      (cond
       [get-alpha?
        (get-alphas-as-mask x y w h bstr width height)]
       [(and (not get-alpha?) (not alpha-channel?))
        ;; For non-alpha mode and no alpha channel; fill in 255s for alpha:
        (for ([j (in-range 0 (min h (- height y)))])
          (let ([row (* j (* 4 w))])
            (for ([i (in-range 0 (min w (- width x)))])
              (let ([p (+ (* 4 i) row)])
                (bytes-set! bstr p 255)))))]))

    (define/public (set-argb-pixels x y w h bstr
                                    [set-alpha? #f]
                                    [pre-mult? #f]
                                    #:unscaled? [unscaled? #f])
      (unless ((bytes-length bstr) . >=  . (* w h 4))
        (raise-mismatch-error (method-name 'bitmap% 'set-argb-pixels)
                              "byte string is too short: "
                              bstr))
      (check-alternate 'set-argb-pixels)
      (cond
       [(and (not unscaled?)
             (not (= backing-scale 1)))
        ;; scale input to match backing:
        (define s backing-scale)
        (define kw (max (*i 1 s) 1))
        (define sw (+ kw (*i (sub1 w) s)))
        (define sh (+ kw (*i (sub1 h) s)))
        (define bstr2 (make-bytes (* sw sh 4)))
        (for ([j (in-range h)])
          (define sj (*i j s))
          (for ([i (in-range w)])
            (define si (*i i s))
            (define p (+ (* j 4 w) (* i 4)))
            (for* ([ik (in-range kw)]
                   [jk (in-range kw)])
              (define p2 (+ (* (+ sj jk) 4 sw) (* (+ si ik) 4)))
              (bytes-set! bstr2 p2 (bytes-ref bstr p))
              (bytes-set! bstr2 (+ p2 1) (bytes-ref bstr (+ p 1)))
              (bytes-set! bstr2 (+ p2 2) (bytes-ref bstr (+ p 2)))
              (bytes-set! bstr2 (+ p2 3) (bytes-ref bstr (+ p 3))))))
        (set-argb-pixels (*i x s) (*i y s) sw sh bstr2 set-alpha? pre-mult? #:unscaled? 1)]
       [(ok?)
        ;; Set pixels:
        (let-values ([(A R G B) (argb-indices)]
                     [(width) (if unscaled? (*i width backing-scale) width)]
                     [(height) (if unscaled? (*i height backing-scale) height)])
          (when (not set-alpha?)
            (surface-flush)
            (let ([data (cairo_image_surface_get_data* s)]
                  [row-width (cairo_image_surface_get_stride s)]
                  [m (and (not pre-mult?) (get-mult-table))])
              (define-syntax-rule (set-loop body)
                (let ([w2 (+ x (min (- width x) w))])
                  (for ([j (in-range y (min (+ y h) height))]
                        [dj (in-naturals)])
                    (let ([row (* j row-width)]
                          [p (* 4 (* dj w))])
                      (for ([i (in-range x w2)])
                        (let* ([4i (unsafe-fx* 4 i)]
                               [pi (unsafe-fx+ p (unsafe-fx* 4 (unsafe-fx- i x)))]
                               [ri (unsafe-fx+ row 4i)])
                          (body pi ri)))))))
              (cond
               [b&w?
                (set-loop
                 (lambda (pi ri)
                   (let ([v (if (and (= (unsafe-bytes-ref bstr (+ pi 1)) 255)
                                     (= (unsafe-bytes-ref bstr (+ pi 2)) 255)
                                     (= (unsafe-bytes-ref bstr (+ pi 3)) 255))
                                255
                                0)])
                     (ptr-set! data _ubyte (unsafe-fx+ ri A) (- 255 v))
                     (ptr-set! data _ubyte (unsafe-fx+ ri 1) v)
                     (ptr-set! data _ubyte (unsafe-fx+ ri 2) v)
                     (ptr-set! data _ubyte (unsafe-fx+ ri B) v))))]
               [alpha-channel?
                (define-syntax-rule (alpha-set-loop pm)
                  (set-loop
                   (lambda (pi ri)
                     (let ([a (bytes-ref bstr pi)])
                       (ptr-set! data _ubyte (unsafe-fx+ ri A) a)
                       (ptr-set! data _ubyte (unsafe-fx+ ri R)
                                 (pm a (unsafe-bytes-ref bstr (unsafe-fx+ pi 1))))
                       (ptr-set! data _ubyte (unsafe-fx+ ri G)
                                 (pm a (unsafe-bytes-ref bstr (unsafe-fx+ pi 2))))
                       (ptr-set! data _ubyte (unsafe-fx+ ri B)
                                 (pm a (unsafe-bytes-ref bstr (unsafe-fx+ pi 3))))))))
                (if m
                    (alpha-set-loop (lambda (a v)
                                      (unsafe-bytes-ref m (unsafe-fx+ (unsafe-fx* a 256) v))))
                    (alpha-set-loop (lambda (a v) (unsafe-fxmin a v))))]
               [else
                (set-loop
                 (lambda (pi ri)
                   (ptr-set! data _ubyte (unsafe-fx+ ri R)
                             (unsafe-bytes-ref bstr (unsafe-fx+ pi 1)))
                   (ptr-set! data _ubyte (unsafe-fx+ ri G)
                             (unsafe-bytes-ref bstr (unsafe-fx+ pi 2)))
                   (ptr-set! data _ubyte (unsafe-fx+ ri B)
                             (unsafe-bytes-ref bstr (unsafe-fx+ pi 3)))))]))
            (cairo_surface_mark_dirty s)))
        (cond
         [(and set-alpha?
               (not alpha-channel?))
          ;; Set alphas:
          (set-alphas-as-mask x y w h bstr (* 4 w) 0
                              (if unscaled? (*i width backing-scale) width)
                              (if unscaled? (*i height backing-scale) height))])
        (drop-alpha-s)]))

    (define/public (get-alphas-as-mask x y w h bstr width height)
      (let ([data (cairo_image_surface_get_data* (if (or b&w? alpha-channel?)
                                                     (begin
                                                       (surface-flush)
                                                       s)
                                                     (begin
                                                       (prep-alpha width height)
                                                       (cairo_surface_flush alpha-s)
                                                       alpha-s)))]
            [row-width (cairo_image_surface_get_stride s)]
            [A (a-index)])
        (for ([j (in-range y (min (+ y h) height))])
          (let ([row (* j row-width)])
            (for ([i (in-range x (min (+ x w) width))])
              (let ([p (* 4 (+ (- i x) (* (- j y) w)))]
                    [q (+ row (* i 4))])
                (bytes-set! bstr p (ptr-ref data _ubyte (+ q A)))))))))

    (define/public (prep-alpha width height)
      (when (and (not b&w?)
                 (not alpha-channel?)
                 s)
        (unless alpha-s-up-to-date?
          (unless alpha-s
            (set! alpha-s (cairo_image_surface_create CAIRO_FORMAT_ARGB32
                                                      width height)))
          (surface-flush)
          (cairo_surface_flush alpha-s)
          (let ([data (cairo_image_surface_get_data* s)]
                [alpha-data (cairo_image_surface_get_data* alpha-s)]
                [row-width (cairo_image_surface_get_stride s)]
                [A (a-index)]
                [B (b-index)])
            (for ([j (in-range height)])
              (let ([row (* j row-width)])
                (for ([i (in-range width)])
                  (let ([q (+ row (* i 4))])
                    (let ([v (quotient
                              (+ (+ (ptr-ref data _ubyte (+ q 1))
                                    (ptr-ref data _ubyte (+ q 2)))
                                 (ptr-ref data _ubyte (+ q B)))
                              3)])
                      (ptr-set! alpha-data _ubyte (+ q A) (- 255 v))))))))
          (cairo_surface_mark_dirty alpha-s)
          (set! alpha-s-up-to-date? #t))))

    (define/private (transparent-white! s width height)
      (let ([bstr (cairo_image_surface_get_data* s)]
            [row-width (cairo_image_surface_get_stride s)]
            [A (a-index)])
        (pointer-fill! bstr 255 (* (cairo_image_surface_get_height s)
                                   (cairo_image_surface_get_stride s)))
        (for ([j (in-range height)])
          (let ([row (* j row-width)])
            (for ([i (in-range width)])
              (ptr-set! bstr _ubyte (+ A (+ row (* i 4))) 0))))))

    (define/public (set-alphas-as-mask x y w h bstr src-w src-A width height)
      (when (or b&w? (and (not b&w?) (not alpha-channel?)))
        (let ([src (if (bytes? bstr) (cast bstr _bytes _pointer) bstr)]
              [data (cairo_image_surface_get_data* s)]
              [row-width (cairo_image_surface_get_stride s)]
              [A (a-index)]
              [B (b-index)])
          (surface-flush)
          (for ([j (in-range y (min (+ y h) height))])
            (let ([row (* j row-width)]
                  [src-row (* (- j y) src-w)])
              (for ([i (in-range x (min (+ x w) width))])
                (let* ([p (+ (* 4 (- i x)) src-row)]
                       [q (+ (* 4 i) row)])
                  (let* ([v (ptr-ref src _ubyte (+ p src-A))]
                         [vv (- 255 v)])
                    (ptr-set! data _ubyte (+ q B) vv)
                    (ptr-set! data _ubyte (+ q 1) vv)
                    (ptr-set! data _ubyte (+ q 2) vv)
                    (ptr-set! data _ubyte (+ q A) (if b&w? v 255)))))))
          (cairo_surface_mark_dirty s))))))

(define (specialize-unknown-kind in kind)
  (define (starts? s)
    (equal? (peek-bytes (bytes-length s) 0 in) s))
  (cond
    [(starts? #"\211PNG\r\n")
     (if (eq? kind 'unknown/alpha)
         'png/alpha
         (if (eq? kind 'unknown/mask)
             'png/mask
             'png))]
    [(starts? #"\xFF\xD8\xFF")
     'jpeg]
    [(starts? #"GIF8")
     'gif]
    [(starts? #"BM")
     (if (eq? kind 'unknown/alpha)
         'bmp/alpha
         'bmp)]
    [(starts? #"#define")
     'xbm]
    [(starts? #"/* XPM */")
     'xpm]
    [else
     ;; unrecognized file type; try to parse as XBM
     'xbm]))

(define/top (make-bitmap [exact-positive-integer? w]
                         [exact-positive-integer? h]
                         [any? [alpha? #t]]
                         #:backing-scale [nonnegative-real? [backing-scale 1.0]])
  (make-object bitmap% w h #f alpha? backing-scale))

(define/top (read-bitmap [(make-alts path-string? input-port?) given-filename]
                         [bitmap-file-kind-symbol? [kind 'unknown/alpha]]
                         [(make-or-false color%) [bg-color #f]]
                         [any? [complain-on-failure? #t]]
                         #:try-@2x? [any? [try-@2x? #f]]
                         #:save-data-from-file? [any? [save-data-from-file? #f]]
                         #:backing-scale [nonnegative-real? [given-backing-scale 1.0]])
  (define-values (filename backing-scale)
    (cond
     [(and try-@2x?
           (path? given-filename)
           (not (memq kind '(unknown/mask gif/mask png/mask))))
      (define new-filename (bytes->path
                            (regexp-replace #rx"([.][^.]*|)$"
                                            (path->bytes given-filename)
                                            #"@2x\\1")))
      (if (file-exists? new-filename)
          (values new-filename (* 2 given-backing-scale))
          (values given-filename given-backing-scale))]
     [else (values given-filename given-backing-scale)]))
  (make-object bitmap% filename kind bg-color complain-on-failure? backing-scale
    save-data-from-file?))

(define/top (make-monochrome-bitmap [exact-positive-integer? w]
                                    [exact-positive-integer? h]
                                    [(make-or-false bytes?) [bits #f]])
  (if bits
      (make-object bitmap% bits w h)
      (make-object bitmap% w h #t)))

(define/top (make-platform-bitmap [exact-positive-integer? w]
                                  [exact-positive-integer? h]
                                  #:backing-scale [nonnegative-real? [backing-scale 1.0]])
  (case (system-type)
    [(macosx) (make-object quartz-bitmap% w h #t backing-scale)]
    [(windows) (make-object win32-no-hwnd-bitmap% w h backing-scale)]
    [(unix) (make-bitmap w h #:backing-scale backing-scale)]))

(define-local-member-name build-cairo-surface)
(define win32-no-hwnd-bitmap%
  (class bitmap%
    (init w h backing-scale)
    (super-make-object (make-alternate-bitmap-kind w h backing-scale))

    (define s (build-cairo-surface w h backing-scale))
    ;; erase the bitmap
    (let ([cr (cairo_create s)])
      (cairo_set_source_rgba cr 1.0 1.0 1.0 1.0)
      (cairo_paint cr)
      (cairo_destroy cr))

    (define/public (build-cairo-surface w h backing-scale)
      (let ([sw (*i backing-scale w)]
	    [sh (*i backing-scale h)])
	(cairo_win32_surface_create_with_dib CAIRO_FORMAT_RGB24 sw sh)))

    (define/override (ok?) #t)
    (define/override (is-color?) #t)
    (define/override (has-alpha-channel?) #f)

    (define/override (get-cairo-surface) s)

    (define/override (release-bitmap-storage)
      (atomically
       (cairo_surface_destroy s)
       (set! s #f)))))

(define quartz-bitmap%
  (class bitmap%
    (init w h [with-alpha? #t] [resolution 1.0] [dest-cg #f])

    (define has-alpha? with-alpha?)
    (define s
      (let* ([sw (*i resolution w)]
             [sh (*i resolution h)]
             [s (if dest-cg
                    (cairo_quartz_surface_create_for_cg_context dest-cg sw sh)
                    (cairo_quartz_surface_create (if with-alpha?
                                                     CAIRO_FORMAT_ARGB32
                                                     CAIRO_FORMAT_RGB24)
                                                 sw
                                                 sh))])
        ;; initialize bitmap to empty - needed?
        (let ([cr (cairo_create s)])
          (cairo_set_operator cr (if with-alpha?
                                     CAIRO_OPERATOR_CLEAR
                                     CAIRO_OPERATOR_SOURCE))
          (cairo_set_source_rgba cr 1.0 1.0 1.0 1.0)
          (cairo_paint cr)
          (cairo_destroy cr))
        s))

    (super-make-object (make-alternate-bitmap-kind w h resolution))

    (define/override (ok?) (and s #t))

    (define/override (is-color?) #t)

    (define/override (has-alpha-channel?) has-alpha?)

    (define/override (get-cairo-surface) s)
    (define/override (get-cairo-alpha-surface)
      (if has-alpha?
          s
          (super get-cairo-alpha-surface)))

    (define/override (release-bitmap-storage)
      (atomically
       (when s
         (cairo_surface_destroy s)
         (set! s #f))))))
