#lang racket/base
(require racket/class
         racket/draw
         rackunit)

(define (check-image path)
  (define (go [solid? #f] [solid-bmp? solid?]
              #:bg-color [bg-color #f])
    (define bm0 (read-bitmap path (if solid? 'unknown 'unknown/alpha)))

    (define w (send bm0 get-width))
    (define h (send bm0 get-height))

    (define bm
      (if bg-color
          (let ([bm (make-bitmap w h)])
            (define dc (send bm make-dc))
            (send dc set-background bg-color)
            (send dc clear)
            (send dc draw-bitmap bm0 0 0)
            bm)
          bm0))

    (define-values (i o) (make-pipe))
    (send bm0 save-file o 'bmp)
    (close-output-port o)

    (define bm2 (read-bitmap i (if solid-bmp? 'unknown 'unknown/alpha) bg-color))

    (when (send bm0 is-color?)
      (when (equal? solid? solid-bmp?)
        (check-equal? (send bm has-alpha-channel?)
                      (send bm2 has-alpha-channel?))))

    (define bstr (make-bytes (* w h 4)))
    (send bm get-argb-pixels 0 0 w h bstr)

    (define bstr2 (make-bytes (* w h 4)))
    (send bm2 get-argb-pixels 0 0 w h bstr2)

    (unless (equal? bstr bstr2)
      (for ([c (in-bytes bstr)]
            [c2 (in-bytes bstr2)])
        (check-equal? c c2))))
  (go #f)
  (go #t)
  (go #t #f)
  (go #f #t #:bg-color (make-color 0 0 255)))

(check-image (collection-file-path "PLT-206.png" "icons"))
(check-image (collection-file-path "heart.png" "icons"))
(check-image (collection-file-path "trumpet.xbm" "icons"))

