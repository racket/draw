#lang racket/base

(require racket/draw
         racket/class
         rackunit)

(test-case
 "make-font is generative without a #:font-list argument"
 (check-not-eq? (make-font) (make-font))
 (check-not-eq? (make-font) (make-font #:font-list the-font-list)))

(test-case
 "make-font caches fonts when given a #:font-list argument"
 (check-eq? (make-font #:font-list the-font-list)
            (make-font #:font-list the-font-list)))

(test-case
 "make-font with different #:font-list arguments returns different fonts"
 (define other-font-list (new font-list%))
 (check-not-eq? (make-font #:font-list the-font-list)
                (make-font #:font-list other-font-list)))

(test-case
 "make-font caches fonts by default when current-font-list is set"
 (parameterize ([current-font-list the-font-list])
   (check-eq? (make-font) (make-font))))
