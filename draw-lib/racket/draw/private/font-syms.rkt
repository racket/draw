#lang racket/base

;; font utilities for contracts

(require racket/contract/base)

(provide family-symbol? style-symbol? weight-symbol? 
         smoothing-symbol? hinting-symbol?
         font-family/c font-weight/c font-style/c
         font-smoothing/c font-hinting/c)

(define (family-symbol? s)
  (memq s '(default decorative roman script
             swiss modern symbol system)))

(define (weight-symbol? s)
  (memq s '(thin ultralight light semilight book normal
                 medium semibold bold ultrabold heavy ultraheavy)))

(define (style-symbol? s)
  (memq s '(normal italic slant)))

(define (smoothing-symbol? s)
  (memq s '(default smoothed unsmoothed partly-smoothed)))

(define (hinting-symbol? s)
  (memq s '(aligned unaligned)))

;; TODO: eventually once all old checks are converted to
;;       contracts, the above can be removed
(define font-family/c (or/c 'default 'decorative 'roman 'script 'swiss
                            'modern 'symbol 'system))

(define font-weight/c (or/c (integer-in 100 1000)
                            'thin 'ultralight 'light 'semilight 'book 'normal
                            'medium 'semibold 'bold 'ultrabold 'heavy 'ultraheavy))
(define font-style/c  (or/c 'normal 'italic 'slant))
(define font-smoothing/c (or/c 'default 'partly-smoothed
                               'smoothed 'unsmoothed))
(define font-hinting/c   (or/c 'aligned 'unaligned))

