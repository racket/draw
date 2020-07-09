#lang racket/base

(module+ test

  ;;========================================================
  ;; Requirements
  ;;========================================================

  (require
    (only-in racket/class
             make-object)
    (only-in "../../../../draw-lib/racket/draw/private/color.rkt"
             color%
             color->immutable-color)
    (only-in rackunit
             check-equal?
             check-not-equal?))

  ;;========================================================
  ;; Definitions
  ;;========================================================

  (define black (make-object color% 0 0 0))
  (define other-black (make-object color% 0 0 0))
  (define red (make-object color% 255 0 0))
  (define green (make-object color% 0 255 0))
  (define blue (make-object color% 0 0 255))
  (define half-red (make-object color% 255 0 0 0.5))
  (define immutable-red (color->immutable-color red))
  
  ;;========================================================
  ;; Equality tests
  ;;========================================================

  (check-equal? black black
                "color should be equal to itself")

  (check-equal? black other-black
                "color should be equal to identical color")

  (check-not-equal? black red
                    "colors with different red values should be unequal")

  (check-not-equal? black green
                    "colors with different green values should be unequal")

  (check-not-equal? black blue
                    "colors with different blue values should be unequal")

  (check-not-equal? red half-red
                    "colors with different alpha values should be unequal")

  (check-equal? red immutable-red
                "mutable and immutable color should be equal when RGBa are equal")
  
  ;;========================================================
  ;; Hash-code tests
  ;;========================================================

  (check-equal? (equal-hash-code black)
                (equal-hash-code other-black)
                "hashes of identical colors should be equal")

  (check-not-equal? (equal-hash-code black)
                    (equal-hash-code red)
                    "colors with different red values should have different hashes")

  (check-not-equal? (equal-hash-code black)
                    (equal-hash-code green)
                    "colors with different green values should have different hashes")

  (check-not-equal? (equal-hash-code black)
                    (equal-hash-code blue)
                    "colors with different blue values should have different hashes")

  (check-not-equal? (equal-hash-code red)
                    (equal-hash-code half-red)
                    "colors with different alpha values should have different hashes")

  (check-equal? (equal-hash-code red)
                (equal-hash-code immutable-red)
                "mutable and immutable color should have equal hashes")
  
  ;;========================================================
  ;; Secondary hash-code tests
  ;;========================================================

  (check-equal? (equal-secondary-hash-code black)
                (equal-secondary-hash-code other-black)
                "secondary hashes of identical colors should be equal")

  (check-not-equal? (equal-secondary-hash-code black)
                    (equal-secondary-hash-code red)
                    "colors with different red values should have different secondary hashes")

  (check-not-equal? (equal-secondary-hash-code black)
                    (equal-secondary-hash-code green)
                    "colors with different green values should have different secondary hashes")

  (check-not-equal? (equal-secondary-hash-code black)
                    (equal-secondary-hash-code blue)
                    "colors with different blue values should have different secondary hashes")

  (check-not-equal? (equal-secondary-hash-code red)
                    (equal-secondary-hash-code half-red)
                    "colors with different alpha values should have different secondary hashes")

  (check-equal? (equal-secondary-hash-code red)
                (equal-secondary-hash-code immutable-red)
                "mutable and immutable color should have equal secondary hashes"))

