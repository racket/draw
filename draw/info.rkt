#lang info

(define collection 'multi)

(define deps '("draw-lib"
               "draw-doc"))
(define implies '("draw-lib"
                  "draw-doc"))

(define pkg-desc "Drawing libraries")

(define pkg-authors '(mflatt))

(define license
  '(Apache-2.0 OR MIT))
