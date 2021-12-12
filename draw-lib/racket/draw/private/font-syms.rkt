#lang racket/base

;; font utilities for contracts

(require racket/contract/base
         racket/match)

(provide family-symbol? style-symbol? weight-symbol? 
         smoothing-symbol? hinting-symbol?
         font-family/c font-weight/c font-style/c
         font-smoothing/c font-hinting/c
         font-feature-settings/c)

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

;; Note: The Pango documentation for `pango_attr_font_features_new` says that it
;; accepts OpenType font features as a string with “the syntax of the CSS
;; font-feature-settings property”. In turn, the CSS spec says that a tag string
;; must be exactly 4 characters in the “U+20–7E codepoint range”.
;;
;; However, in reality, Pango passes this string to HarfBuzz’s
;; `hb_feature_from_string` function, which does not implement CSS string
;; escapes. This means it’s impossible to correctly format a feature string for
;; a feature with a tag like ‘a'"b’ because there is no way to properly escape
;; both #\' and #\".
;;
;; However, in practice, all OpenType feature tags are strictly alphanumeric,
;; anyway. So we just disallow the " character, which avoids the problem.
(define font-feature-tag/c (and/c string? #px"^[ !#-~]{4}$"))
(define font-feature-settings/c (and/c hash-equal?
                                       hash-strong?
                                       (hash/c font-feature-tag/c
                                               exact-nonnegative-integer?
                                               #:immutable #t)))
