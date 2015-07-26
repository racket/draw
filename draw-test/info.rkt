#lang info

(define collection 'multi)

(define deps '("base"))
(define build-deps '("racket-index"
                     "scheme-lib"
                     "draw-lib"
                     "racket-test"
                     "sgl"
                     "gui-lib"
                     "rackunit-lib"
                     "pconvert-lib"
                     "compatibility-lib"
                     "sandbox-lib"))
(define update-implies '("draw-lib"))

(define pkg-desc "tests for \"draw\"")

(define pkg-authors '(mflatt))
