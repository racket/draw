#lang racket/base
(require ffi/unsafe
         ffi/unsafe/objc
         ffi/unsafe/define
         ffi/unsafe/nsstring
         ffi/unsafe/alloc
         racket/class
         "local.rkt"
         "bitmap.rkt"
         "../unsafe/cairo.rkt"
         "emoji-sequences.rkt")

;; On Mac OS, Pango+Cairo doesn't handle emoji, so we implement emoji
;; rendering directly with CoreText primitives. Emoji modifiers are
;; supported when rendering text in "combined" mode, but not when
;; rendering charcter-by-character (since a modifier is a Unicode code
;; point, so treated as a separate character).

(provide emoji?
         find-emoji-sequence
         emoji-extent
         draw-emoji)

(define (emoji? ch)
  (cond
    [emoji-sequences
     (define v (hash-ref emoji-sequences (char->integer ch) #f))
     (and v
          (or (eq? v #t)
              (hash-ref v #f #f)))]
    [else #f]))

;; Returns #f or (cons start end) for the first emoji sequence in `s`
(define (find-emoji-sequence s start)
  (cond
    [emoji-sequences
     (define len (string-length s))
     (let loop ([i start])
       (cond
         [(= i len) #f]
         [else
          (or (let table-loop ([table emoji-sequences] [j i])
                (define v (hash-ref table (char->integer (string-ref s j)) #f))
                (cond
                  [(not v) #f]
                  [(eq? v #t) (cons i (add1 j))]
                  [else
                   (define new-j (add1 j))
                   (or (cond
                         [(= new-j len) #f]
                         [else (table-loop v new-j)])
                       ;; The following two cases are dedundant for
                       ;; characters in the middle of the search range,
                       ;; but only one for an end-of-string and the other
                       ; for start-of-start
                       (and (hash-ref v #f #f)
                            (cons i new-j))
                       (and (hash-ref table #f #f)
                            (cons i j)))]))
              (loop (add1 i)))]))]
    [else #f]))

(define-values (emoji-extent draw-emoji)
  (cond
    [(eq? 'macosx (system-type))

     (define cf-lib (ffi-lib "/System/Library/Frameworks/CoreFoundation.framework/CoreFoundation"))
     (define ct-lib (ffi-lib "/System/Library/Frameworks/CoreText.framework/CoreText" #:fail (lambda () #f)))
     (define cg-lib (ffi-lib "/System/Library/Frameworks/CoreGraphics.framework/CoreGraphics" #:fail (lambda () #f)))

     (cond
       [(and ct-lib cg-lib)
        (define-ffi-definer define-cf cf-lib)
        (define-ffi-definer define-ct ct-lib)
        (define-ffi-definer define-cg cg-lib)

        (define _CFIndex _long)

        (define-cpointer-type _CFDictionaryRef)
        (define-cf CFDictionaryCreate (_fun (_pointer = #f) (_ptr i _pointer) (_ptr i _pointer) (_CFIndex = 1) (_pointer = #f) (_pointer = #f) -> _CFDictionaryRef))

        (define-cpointer-type _CFAttributedStringRef)
        (define-cf CFAttributedStringCreate (_fun (_pointer = #f) _NSString _CFDictionaryRef -> _CFAttributedStringRef))

        (define-cf CFRelease (_fun _pointer -> _void))


        (define _CGFloat (if (eqv? 64 (system-type 'word)) _double _float))
        (define-cstruct _CGAffineTransform ([a _CGFloat]
                                            [b _CGFloat]
                                            [c _CGFloat]
                                            [d _CGFloat]
                                            [tx _CGFloat]
                                            [ty _CGFloat]))

        (define-cpointer-type _CGFontRef)
        (define-cg CGFontCreateWithFontName (_fun _NSString -> _CGFontRef)
          #:wrap (allocator CFRelease))

        (define-cpointer-type _CTFontRef)
        (define-ct CTFontCreateWithGraphicsFont (_fun _CGFontRef _CGFloat (_pointer = #f) (_pointer = #f) -> _CTFontRef)
          #:wrap (allocator CFRelease))

        (define _CGContextRef (_cpointer 'CGContextRef))

        (define-cg CGContextSaveGState (_fun _CGContextRef -> _void))
        (define-cg CGContextRestoreGState (_fun _CGContextRef -> _void))

        (define-cg CGContextTranslateCTM (_fun _CGContextRef _CGFloat _CGFloat -> _void))
        (define-cg CGContextScaleCTM (_fun _CGContextRef _CGFloat _CGFloat -> _void))
        (define-cg CGContextConcatCTM (_fun _CGContextRef _CGAffineTransform -> _void))

        (define-cg CGContextSetTextPosition (_fun _CGContextRef _CGFloat _CGFloat -> _void))

        (define-cpointer-type _CTLineRef)
        (define-ct CTLineCreateWithAttributedString (_fun _CFAttributedStringRef -> _CTLineRef))
        (define-ct CTLineDraw (_fun _CTLineRef _CGContextRef -> _void))

        (define-ct CTLineGetTypographicBounds (_fun _CTLineRef
                                                    (a : (_ptr o _CGFloat))
                                                    (d : (_ptr o _CGFloat))
                                                    (l : (_ptr o _CGFloat))
                                                    -> (w : _double)
                                                    -> (values w a d l)))

        (define-ct kCTFontAttributeName _pointer)

        (define emoji-cg (CGFontCreateWithFontName "Apple Color Emoji"))

        (define emoji-ct-cache (make-hasheqv))

        (define emoji-bm-cache (make-weak-hash))

        (define (font->ct font)
          (define size (exact->inexact (send font get-point-size)))
          (or (hash-ref emoji-ct-cache size #f)
              (let ([ct (CTFontCreateWithGraphicsFont emoji-cg size)])
                (hash-set! emoji-ct-cache size ct)
                ct)))

        (define (text->line font ch-or-str)
          (define emoji-ct (font->ct font))

          (define str (if (string? ch-or-str)
                          ch-or-str
                          (string ch-or-str)))

          (define font-dict (CFDictionaryCreate kCTFontAttributeName emoji-ct))
          (define attr-str (CFAttributedStringCreate str font-dict))
          (define line (CTLineCreateWithAttributedString attr-str))
          (CFRelease attr-str)
          (CFRelease font-dict)

          line)

        ;; in atomic mode
        (define (emoji-extent font ch-or-str)
          (define line (text->line font ch-or-str))
          (define-values (w a d l) (CTLineGetTypographicBounds line))

          (CFRelease line)

          ;; Dropping leading, because that's what Pango does
          (values w (+ a d) d 0.0))

        (define (draw-emoji dc cr x y font ch-or-str)
          (define line (text->line font ch-or-str))
          (let draw-loop ([cr cr] [x x] [y y])
            (cond
              [(not (cairo_quartz_surface_get_cg_context (cairo_get_target cr)))
               ;; Draw to a bitmap, then copy to cr
               (define key (cons ch-or-str (send font get-point-size)))
               (define bm+a (or (hash-ref emoji-bm-cache ch-or-str #f)
                                (let ()
                                  (define-values (w a d l) (CTLineGetTypographicBounds line))
                                  (define bm (make-platform-bitmap (inexact->exact (ceiling w))
                                                                   (inexact->exact (ceiling (+ a d)))
                                                                   #:backing-scale 2.0))
                                  (send (send bm make-dc) in-cairo-context
                                        (lambda (bm-cr)
                                          (draw-loop bm-cr 0.0 a)))
                                  (define bm+a (cons bm a))
                                  (hash-set! emoji-bm-cache key bm+a)
                                  bm+a)))
               (define bm (car bm+a))
               (define ty (- y (cdr bm+a)))
               (define w (send bm get-width))
               (define h (send bm get-height))
               (let ([old-s (cairo_get_source cr)])
                 (cairo_pattern_reference old-s)
                 (cairo_set_source_surface cr (send bm get-cairo-surface) x ty)
                 (let ([m (make-cairo_matrix_t 0.0 0.0 0.0 0.0 0.0 0.0)])
                   (cairo_matrix_init_translate m 0 0)
                   (cairo_matrix_scale m 2.0 2.0)
                   (cairo_matrix_translate m (- x) (- ty))
                   (cairo_pattern_set_matrix (cairo_get_source cr) m))
                 (cairo_pattern_set_filter (cairo_get_source cr) CAIRO_FILTER_NEAREST)
                 (cairo_new_path cr)
                 (cairo_rectangle cr x ty w h)
                 (cairo_fill cr)
                 (cairo_set_source cr old-s)
                 (cairo_pattern_destroy old-s))]
              [else
               ;; Convert Cairo transformation matrix to CG matrix
               (define mx (make-cairo_matrix_t 1 0 0 1 0 0))
               (cairo_get_matrix cr mx)
               (define tm (make-CGAffineTransform (cairo_matrix_t-xx mx)
                                                  (cairo_matrix_t-yx mx)
                                                  (cairo_matrix_t-xy mx)
                                                  (cairo_matrix_t-yy mx)
                                                  (cairo_matrix_t-x0 mx)
                                                  (cairo_matrix_t-y0 mx)))

               ;; Extract CG context from Cairo, installing the Cairo
               ;; clipping state as CG clipping
               (define s (cairo_get_target cr))
               (cairo_surface_flush s)
               (define cg (cairo_quartz_get_cg_context_with_clip cr))

               ;; Set CG transformation to match Cairo plus displacement for glyph
               (CGContextSaveGState cg)
               (CGContextConcatCTM cg tm)
               (CGContextTranslateCTM cg
                                      (exact->inexact x)
                                      (exact->inexact y))
               (CGContextScaleCTM cg 1.0 -1.0)

               ;; Actually draw the glpyh
               (CGContextSetTextPosition cg 0.0 0.0)
               (CTLineDraw line cg)

               (CFRelease line)

               ;; Go back to Cairo mode for the CG
               (CGContextRestoreGState cg)
               (cairo_quartz_finish_cg_context_with_clip cr)
               (cairo_surface_mark_dirty s)])))

        (values emoji-extent draw-emoji)]
       [else
        ;; can't draw emoji on old Mac OS
        (values
         (lambda args (values 12.0 12.0 12.0 0.0))
         void)])]
    [else (values void void)]))

;; This submodule parses "emoji-sequences.txt", "emoji-zwj-sequences.txt",
;; "emoji-variation-sequences.txt", and "emoji-data.txt" from Unicode
;; to implement "emoji-sequences.rkt".
(module extract-emoji-sequences racket/base
  (require racket/pretty)

  (define emoji-presentation (make-hasheqv))
  (call-with-input-file*
   "emoji-data.txt"
   (lambda (i)
     (let loop ()
       (define l (read-line i))
       (unless (eof-object? l)
         (cond
           [(regexp-match #px"^([0-9A-F]+)[.][.]([0-9A-F]+) +; Emoji_Presentation" l)
            => (lambda (m)
                 (for ([i (in-range (string->number (cadr m) 16)
                                    (add1 (string->number (caddr m) 16)))])
                   (hash-set! emoji-presentation i #t)))]
           [(regexp-match #px"^([0-9A-F]+) +; Emoji_Presentation" l)
            => (lambda (m)
                 (hash-set! emoji-presentation (string->number (cadr m) 16) #t))])
         (loop)))))

  (define table (make-hasheqv))

  (define (hash-add! ht k v)
    (when (hash-ref ht k #f)
      (unless (equal? (hash-ref ht k #f) v)
        (error "collision building table" k ht)))
    (hash-set! ht k v))

  (define (parse-sequences i)
    (let loop ()
      (define l (read-line i))
      (unless (eof-object? l)
        (cond
          [(regexp-match #px"^([0-9A-F]+)[.][.]([0-9A-F]+)" l)
           => (lambda (m)
                (for ([i (in-range (string->number (cadr m) 16)
                                   (add1 (string->number (caddr m) 16)))])
                  (define t (hash-ref table i (lambda () (make-hasheqv))))
                  (hash-set! table i t)
                  (hash-set! t #f #t)))]
          [(regexp-match? #px"^([0-9A-F]+)" l)
           (let loop ([l l] [table table] [prev #f])
             (cond
               [(regexp-match-positions #px"^[0-9A-F]+" l)
                => (lambda (m)
                     (define i (string->number (substring l (caar m) (cdar m)) 16))
                     (define t (hash-ref table i (lambda () (make-hasheqv))))
                     (hash-add! table i t)
                     (loop (substring l (cdar m)) t (or prev i))
                     (when (and (= i #xFE0F)
                                (or (and prev
                                         (prev . >= . #x1f000))
                                    (hash-ref emoji-presentation prev #f)))
                       ;; treat U+FE0F as optional after Emoji_Presentation characters
                       (for ([(k v) (in-hash t)])
                         (hash-add! table k v))))]
               [(regexp-match-positions #px"^\\s" l)
                (loop (substring l 1) table prev)]
               [else (hash-set! table #f #t)]))])
        (loop))))

  (call-with-input-file "emoji-sequences.txt" parse-sequences)
  (call-with-input-file "emoji-zwj-sequences.txt" parse-sequences)
  (call-with-input-file "emoji-variation-sequences.txt" parse-sequences)

  (let simplify ([table table])
    (for ([(k v) (in-hash table)])
      (when (hash? v)
        (simplify v)
        (when (and (= (hash-count v) 1)
                   (hash-ref v #f #f))
          (hash-set! table k #t)))))

  (pretty-write `(module emoji-sequences racket/base
                   (provide emoji-sequences)
                   (define emoji-sequences
                     (and (eq? 'macosx (system-type))
                          ,table)))))
