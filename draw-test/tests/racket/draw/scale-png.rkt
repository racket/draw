#lang racket/base
(require racket/class
         racket/draw
         racket/gui/base
         racket/file)

(define (do-scale-test make-orig-bitmap)
  (define bm (make-orig-bitmap 20 20))
  (define dc (send bm make-dc))

  (send dc draw-rectangle 0 0 19 19)

  (define fn (make-temporary-file))
  (void (send bm save-file fn 'png #:unscaled? #t))

  (define bm2 (read-bitmap fn))
  (delete-file fn)

  (define bm3 (make-bitmap 20 20 #:backing-scale (send bm get-backing-scale)))
  (define dc3 (send bm3 make-dc))

  (void (send dc3 draw-bitmap bm 0 0))

  (define (s v)
    (inexact->exact (ceiling (* v (send bm3 get-backing-scale)))))

  (define bstr2 (make-bytes (* 4 (send bm2 get-width) (send bm2 get-height))))
  (define bstr3 (make-bytes (* 4 (s (send bm3 get-width)) (s (send bm3 get-height)))))

  (send bm2 get-argb-pixels 0 0 (send bm2 get-width) (send bm2 get-height) bstr2 #:unscaled? #t)
  (send bm3 get-argb-pixels 0 0 (s (send bm3 get-width)) (s (send bm3 get-height)) bstr3 #:unscaled? #t)

  (unless (equal? bstr2 bstr3)
    (error "scaled-bitmap PNG problem" make-orig-bitmap)))

(do-scale-test make-screen-bitmap)
(do-scale-test (lambda (w h) (make-platform-bitmap w h #:backing-scale 2)))
(do-scale-test (lambda (w h) (make-bitmap w h #:backing-scale 2)))
