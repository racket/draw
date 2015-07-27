#lang racket/base
(require racket/port)

(provide read-bmp
         write-bmp1
         write-bmp24
         write-bmp32)

(define BI_RGB 0)
(define BI_RLE8 1)
(define BI_RLE4 2)
(define BI_BITFIELDS 3)

(define (read-byte* in)
  (let ([c (read-byte in)])
    (if (eof-object? c)
        (error 'read-bmp "unexpected end of BMP stream: ~e" in)
        c)))

(define (int4 in)
  (+ (read-byte* in)
     (arithmetic-shift (read-byte* in) 8)
     (arithmetic-shift (read-byte* in) 16)
     (arithmetic-shift (read-byte* in) 24)))

(define (int2 in)
  (+ (read-byte* in)
     (arithmetic-shift (read-byte* in) 8)))

(define (mask->shift v)
  (cond
   [(zero? v) 0]
   [(bitwise-bit-set? v 0) 0]
   [else (sub1 (mask->shift (arithmetic-shift v -1)))]))

(define (make-rle8-port in)
  (make-input-port/read-to-peek
   (object-name in)
   (let ([remaining 0]
         [value 0]
         [absolute 0]
         [abs-skip? #f])
     (lambda (bstr)
       (cond
        [(positive? absolute)
         (let ([v (read-byte* in)])
           (set! absolute (sub1 absolute))
           (when (zero? absolute)
             (when abs-skip?
               (read-byte* in)))
           (bytes-set! bstr 0 v)
           1)]
        [(zero? remaining)
         (let ([r (read-byte in)])
           (if (eof-object? r)
               r
               (if (zero? r)
                   ;; special:
                   (let ([c (read-byte* in)])
                     (case c
                       [(0) 0] ; end-of-line
                       [(1) 0] ; end-of-bitmap
                       [(2) (error 'read-bmp 
                                   "RLE8 cursor command not supported in stream: ~e" 
                                   in)]
                       [else
                        (set! absolute c)
                        (set! abs-skip? (odd? c))
                        0]))
                   ;; normal encoding:
                   (let ([v (read-byte* in)])
                     (set! remaining (sub1 r))
                     (set! value v)
                     (bytes-set! bstr 0 v)
                     1))))]
        [else
         (set! remaining (sub1 remaining))
         (bytes-set! bstr 0 value)
         1])))
   #f
   void))

(define (make-rle4-port in)
  (make-input-port/read-to-peek
   (object-name in)
   (let ([remaining 0]
         [value 0]
         [absolute 0]
         [abs-skip? #f]
         [nibble #f]) ; leftover half-byte to be delivered
     (lambda (bstr)
       (let ([nibbles
              (cond
               [(positive? absolute)
                (let ([v (read-byte* in)])
                  (set! absolute (- absolute 2))
                  (when (absolute . < . 1)
                    (when abs-skip?
                      (read-byte* in)))
                  (bytes-set! bstr 0 v)
                  (if (absolute . < . 0)
                      1
                      2))]
               [(zero? remaining)
                (let ([r (read-byte in)])
                  (if (eof-object? r)
                      r
                      (if (zero? r)
                          ;; special:
                          (let ([c (read-byte* in)])
                            (case c
                              [(0) 0] ; end-of-line
                              [(1) 0] ; end-of-bitmap
                              [(2) (error 'read-bmp 
                                          "RLE8 cursor command not supported in stream: ~e" 
                                          in)]
                              [else
                               (set! absolute c)
                               (set! abs-skip? (positive? (bitwise-and c 3)))
                               0]))
                          ;; normal encoding:
                          (let ([v (read-byte* in)])
                            (set! remaining r)
                            (set! value v)
                            0))))]
               [(= remaining 1)
                (bytes-set! bstr 0 value)
                1]
               [else
                (set! remaining (- remaining 2))
                (bytes-set! bstr 0 value)
                2])])
         (cond
          [(eof-object? nibbles)
           (if nibble
               (begin
                 (bytes-set! bstr 0 (arithmetic-shift nibble 4))
                 (set! nibble #f)
                 1)
               nibbles)]
          [(zero? nibbles)
           0]
          [(and (not nibble) (= 2 nibbles))
           1]
          [(and (not nibble) (= 1 nibbles))
           (set! nibble (bitwise-and (bytes-ref bstr 0) #xF0))
           0]
          [(and nibble (= 1 nibbles))
           (bytes-set! bstr 0 (bitwise-ior nibble
                                           (arithmetic-shift (bytes-ref bstr 0) -4)))
           (set! nibble #f)
           1]
          [else ;; (and nibble (= 2 nibbles))
           (let ([old nibble])
             (set! nibble (arithmetic-shift (bitwise-and (bytes-ref bstr 0) #xF) 4))
             (bytes-set! bstr 0 (bitwise-ior old
                                             (arithmetic-shift (bytes-ref bstr 0) -4)))
             1)]))))
   #f
   void))

(define (read-bmp in #:background [bg-color (list 255 255 255)])
  (unless (and (= (read-byte* in) (char->integer #\B))
               (= (read-byte* in) (char->integer #\M)))
    (error 'read-bmp "not a BMP stream: ~e" in))
  (let ([file-size (int4 in)]
        [reserved1 (int2 in)]
        [reserved2 (int2 in)]
        [offset (int4 in)])
    ;; Start DIB header
    (let ([header-size (int4 in)])
      (unless (or (= header-size 40)
                  (= header-size 12)
                  (= header-size 124))
        (error 'read-bmp "expected a 12-, 40-, or 124-byte DIB header, got ~a in stream: ~e" header-size in))
      (let-values ([(width height bits-per-pixel compression color-count padded-rgb?
                           ;; For 32-bit mode:
                           r-mask g-mask b-mask a-mask)
                    (case header-size
                      [(12)
                       (let ([width (int2 in)]
                             [height (int2 in)]
                             [planes (int2 in)]
                             [bits-per-pixel (int2 in)])
                         (values width height bits-per-pixel BI_RGB 0 #f
                                 0 0 0 0))]
                      [else
                       (let ([width (int4 in)]
                             [height (int4 in)]
                             [planes (int2 in)]
                             [bits-per-pixel (int2 in)]
                             [compression (int4 in)]
                             [image-size (int4 in)]
                             [hres (int4 in)]
                             [vres (int4 in)]
                             [color-count (int4 in)]
                             [colors-used (int4 in)])
                         (unless (or (= compression BI_RGB)
                                     (= compression BI_RLE4)
                                     (= compression BI_RLE8)
                                     (= compression BI_BITFIELDS))
                           (error 'read-bmp "unsupported compression type ~a in stream: ~e" compression in))
                         (let-values ([(r-mask b-mask g-mask a-mask)
                                       (if (= header-size 40)
                                           (values #xff #xff00 #xff000 0)
                                           (values (int4 in) (int4 in) (int4 in) (int4 in)))])
                           (when (header-size . > . 40)
                             (read-bytes (- header-size 56) in))
                           (values width height bits-per-pixel compression color-count #t
                                   r-mask b-mask g-mask a-mask)))])])
        (let* ([color-count (if (zero? color-count)
                                (arithmetic-shift 1 bits-per-pixel)
                                color-count)]
               [colors
                (if (bits-per-pixel . >= . 16)
                    #f
                    (let ([vec (make-vector color-count #"\0\0\0\xFF")])
                      (for ([i (in-range color-count)])
                        (let ([b (read-byte* in)]
                              [g (read-byte* in)]
                              [r (read-byte* in)])
                          (when padded-rgb? (read-byte* in))
                          (vector-set! vec i (bytes r g b 255))))
                      vec))]
               [current-pos (+ 14
                               header-size
                               (if colors (* color-count (if padded-rgb? 4 3)) 0))])
          ;; Image data:
          (read-bytes (- offset current-pos) in)
          (let ([in (cond
                     [(= compression BI_RLE4) (make-rle4-port in)]
                     [(= compression BI_RLE8) (make-rle8-port in)]
                     [else in])])
            (values
             width
             height
             (list->vector
              (reverse
               (for/list ([j (in-range height)])
                 (let* ([row (make-bytes (* 4 width) 255)]
                        [install-color!
                         (lambda (i c)
                           (if (c . < . color-count)
                               (let ([col (vector-ref colors c)])
                                 (bytes-set! row (* i 4) (bytes-ref col 0))
                                 (bytes-set! row (+ 1 (* i 4)) (bytes-ref col 1))
                                 (bytes-set! row (+ 2 (* i 4)) (bytes-ref col 2)))
                               (error 'read-bmp "bad color table index ~a in stream: ~e" c in)))])
                   (case bits-per-pixel
                     [(32)
                      (if (and (= r-mask #xff)
                               (= g-mask #xff00)
                               (= b-mask #xff000)
                               (= a-mask 0))
                          (for ([i (in-range width)])
                            (let ([b (read-byte* in)]
                                  [g (read-byte* in)]
                                  [r (read-byte* in)])
                              (read-byte* in) ; discard
                              (bytes-set! row (* i 4) r)
                              (bytes-set! row (+ 1 (* i 4)) g)
                              (bytes-set! row (+ 2 (* i 4)) b)))
                          (let ([r-shift (mask->shift r-mask)]
                                [g-shift (mask->shift g-mask)]
                                [b-shift (mask->shift b-mask)]
                                [a-shift (mask->shift a-mask)])
                            (for ([i (in-range width)])
                              (let ([v (+ (read-byte* in)
                                          (arithmetic-shift (read-byte* in) 8)
                                          (arithmetic-shift (read-byte* in) 16)
                                          (arithmetic-shift (read-byte* in) 24))])
                                (define a (arithmetic-shift (bitwise-and v a-mask) a-shift))
                                (define (adj sel v) (if bg-color
                                                        (cond
                                                         [(= a 0) (sel bg-color)]
                                                         [(= a 255) v]
                                                         [else
                                                          (inexact->exact
                                                           (round
                                                            (+ (/ (* v a) 255.0)
                                                               (/ (* (sel bg-color) (- 255 a)) 255.0))))])
                                                        v))
                                (bytes-set! row (* i 4) (adj car (arithmetic-shift (bitwise-and v r-mask) r-shift)))
                                (bytes-set! row (+ 1 (* i 4)) (adj cadr (arithmetic-shift (bitwise-and v g-mask) g-shift)))
                                (bytes-set! row (+ 2 (* i 4)) (adj caddr (arithmetic-shift (bitwise-and v b-mask) b-shift)))
                                (bytes-set! row (+ 3 (* i 4)) (if bg-color 255 a))))))]
                     [(24)
                      (for ([i (in-range width)])
                        (let ([b (read-byte* in)]
                              [g (read-byte* in)]
                              [r (read-byte* in)])
                          (bytes-set! row (* i 4) r)
                          (bytes-set! row (+ 1 (* i 4)) g)
                          (bytes-set! row (+ 2 (* i 4)) b)))]
                     [(16)
                      (for ([i (in-range width)])
                        (let ([col (bitwise-ior (read-byte* in)
                                                (arithmetic-shift (read-byte* in) 8))])
                          (bytes-set! row (* i 4) (arithmetic-shift (bitwise-and col #x7C00) -7))
                          (bytes-set! row (+ 1 (* i 4)) (arithmetic-shift (bitwise-and col #x3E0) -2))
                          (bytes-set! row (+ 2 (* i 4)) (arithmetic-shift (bitwise-and col #x1F) 3))))]
                     [(8)
                      (for ([i (in-range width)])
                        (install-color! i (read-byte* in)))]
                     [(4)
                      (for/fold ([b 0]) ([i (in-range width)])
                        (let ([b (if (zero? (bitwise-and i 1))
                                     (read-byte* in)
                                     (arithmetic-shift b 4))])
                          (install-color! i (arithmetic-shift (bitwise-and b #xF0) -4))
                          b))]
                     [(1)
                      (for/fold ([b 0]) ([i (in-range width)])
                        (let ([b (if (zero? (bitwise-and i 7))
                                     (read-byte* in)
                                     (arithmetic-shift b 1))])
                          (install-color! i (arithmetic-shift (bitwise-and b #x80) -7))
                          b))]
                     [else
                      (error 'read-bmp "unsupported bits-per-pixel count ~a in stream: ~e"
                             bits-per-pixel in)])
                   ;; skip padding, if any:
                   (when (= compression BI_RGB)
                     (let ([n (modulo (ceiling (/ (* width bits-per-pixel) 8)) 4)])
                       (unless (zero? n)
                         (read-bytes (- 4 n) in))))
                   row)))))))))))

;; ----------------------------------------

(define (int4-bstr v)
  (integer->integer-bytes v 4 #f #f))

(define (int2-bstr v)
  (integer->integer-bytes v 2 #f #f))

(define (bmp-header data-size
                    #:offset [offset (+ 14 40)])
  (bytes-append #"BM"
                (int4-bstr (+ data-size offset))
                (int4-bstr 0)
                (int4-bstr offset)))

(define (bitmapinfo-header w h
                           #:header-size [header-size 40]
                           #:horiz-res [h-res 2835]
                           #:vertical-res [v-res 2835]
                           #:bits-per-pixel [bits-per-pixel 24]
                           #:image-format [image-format BI_RGB]
                           #:image-size [image-size 0]
                           #:color-palette-size [color-palette-size 0])
  (bytes-append (int4-bstr header-size)
                (int4-bstr w)
                (int4-bstr h)
                (int2-bstr 1) ; color planes
                (int2-bstr bits-per-pixel)
                (int4-bstr image-format)
                (int4-bstr image-size)
                (int4-bstr h-res)
                (int4-bstr v-res)
                (int4-bstr color-palette-size)
                (int4-bstr 0))) ; important colors

(define (bitmapv4-header32 w h
                           #:header-size [header-size 108]
                           #:horiz-res [h-res 2835]
                           #:vertical-res [v-res 2835]
                           #:image-size image-size)
  (bytes-append (bitmapinfo-header w h
                                   #:header-size header-size
                                   #:horiz-res h-res
                                   #:vertical-res v-res
                                   #:bits-per-pixel 32
                                   #:image-format BI_BITFIELDS
                                   #:image-size image-size)
                (int4-bstr #x00FF0000)
                (int4-bstr #x0000FF00)
                (int4-bstr #x000000FF)
                (int4-bstr #xFF000000)
                #"BGRs"
                (make-bytes 36) ; color space
                (int4-bstr 0) ; red gamma
                (int4-bstr 0) ; green gamma
                (int4-bstr 0))) ; blue gamma

(define (bitmapv5-header32 w h
                           #:header-size [header-size 124]
                           #:horiz-res [h-res 2835]
                           #:vertical-res [v-res 2835]
                           #:image-size image-size)
  (bytes-append (bitmapv4-header32 w h
                                   #:header-size header-size
                                   #:horiz-res h-res
                                   #:vertical-res v-res
                                   #:image-size image-size)
                (int4-bstr 2) ; intent?
                (int4-bstr 0)
                (int4-bstr 0)
                (int4-bstr 0)))

(define (align r)
  (if (zero? (modulo r 4))
      r
      (+ r (- 4 (modulo r 4)))))

(define (write-bmp1 bstr w h [o (current-output-port)])
  (define row-size (align (quotient (+ w 7) 8)))
  (define size (* h row-size))
  (write-bytes (bmp-header size #:offset (+ 14 40 8))
               o)
  (write-bytes (bitmapinfo-header w h
                                  #:bits-per-pixel 1
                                  #:color-palette-size 2)
               o)
  ;; color table: black then white
  (write-bytes (bytes 0 0 0 0
                      #xFF #xFF #xFF 0)
               o)

  (define 1-bstr (make-bytes size))
  (for ([j (in-range h)])
    (for ([i (in-range 0 w 8)])
      (define v
        (for/fold ([v 0]) ([k (in-range (min 8 (- w i)))])
          (define p (* 4 (+ i k (* j w))))
          (+ (arithmetic-shift v 1)
             (if (and (= 255 (bytes-ref bstr (+ p 1)))
                      (= 255 (bytes-ref bstr (+ p 2)))
                      (= 255 (bytes-ref bstr (+ p 3))))
                 1
                 0))))
      (define q (+ (quotient i 8) (* (- h j 1) row-size)))
      (bytes-set! 1-bstr q v)))
  (write-bytes 1-bstr o)
  (void))

(define (write-bmp24 bstr w h [o (current-output-port)])
  (define row-size (align  (* w 3)))
  (define size (* h row-size))
  (write-bytes (bmp-header size) o)
  (write-bytes (bitmapinfo-header w h) o)

  (define 24-bstr (make-bytes size))
  (for* ([j (in-range h)]
         [i (in-range w)])
    (define p (* 4 (+ i (* j w))))
    (define q (+ (* 3 i) (* (- h j 1) row-size)))
    (bytes-set! 24-bstr (+ q 2) (bytes-ref bstr (+ p 1)))
    (bytes-set! 24-bstr (+ q 1) (bytes-ref bstr (+ p 2)))
    (bytes-set! 24-bstr q (bytes-ref bstr (+ p 3))))
  (write-bytes 24-bstr o)
  (void))

(define (write-bmp32 bstr w h [o (current-output-port)])
  (define row-size (* w 4))
  (define size (* row-size h))
  (write-bytes (bmp-header size #:offset (+ 14 124)) o)
  (write-bytes (bitmapv5-header32 w h #:image-size size) o)
  
  (define 32-bstr (make-bytes size))
  (for* ([j (in-range h)]
         [i (in-range w)])
    (define p (* 4 (+ i (* j w))))
    (define q (+ (* 4 i) (* (- h j 1) row-size)))
    (bytes-set! 32-bstr (+ q 3) (bytes-ref bstr p))
    (bytes-set! 32-bstr (+ q 2) (bytes-ref bstr (+ p 1)))
    (bytes-set! 32-bstr (+ q 1) (bytes-ref bstr (+ p 2)))
    (bytes-set! 32-bstr (+ q 0) (bytes-ref bstr (+ p 3))))

  (write-bytes 32-bstr o)
  (void))
