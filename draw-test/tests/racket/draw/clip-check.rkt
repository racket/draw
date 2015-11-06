#lang racket/base
(require racket/draw
         racket/class)

;; Check for a bug in Cairo that we've explicitly patched:

(define dc (new bitmap-dc% [bitmap (make-object bitmap% 100 100)]))
(define pp (send dc get-pen))
(let ([p (send dc get-pen)])
  (send dc set-pen (send the-pen-list find-or-create-pen (send p get-color)
                         1
                         'dot
                         (send p get-cap)
                         (send p get-join))))
(define fill (new region% [dc dc]))
(send fill set-ellipse 10 10 10 10)
(send dc set-clipping-region fill)
(send dc draw-rectangle 0 0 100 50)
