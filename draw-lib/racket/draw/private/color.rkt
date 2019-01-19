#lang racket/base
(require racket/class
         racket/contract/base
         (except-in "syntax.rkt" real-in integer-in)
         "lock.rkt"
         (for-syntax racket/base
                     racket/local
                     racket/list
                     racket/match))

(provide color%
         make-color
         color-red
         color-green
         color-blue
         color-alpha
         color-database<%>
         the-color-database
         color->immutable-color)

(define-local-member-name
  r g b a
  set-immutable
  s-immutable?)

(define color%
  (class object%
    (field [r 0]
           [g 0]
           [b 0]
           [a 1.0])
    (field [s-immutable? #f])

    (init-rest args)
    (super-new)
    (case-args
     args
     [() (void)]
     [([string? s])
      (let ([v (hash-ref colors (string-foldcase s) #f)])
        (if v
            (begin
              (set! r (vector-ref v 0))
              (set! g (vector-ref v 1))
              (set! b (vector-ref v 2)))
            (error 'color% "unknown color name: ~e" (car args))))]
     [([color% c])
      (set! r (color-red c))
      (set! g (color-green c))
      (set! b (color-blue c))
      (set! a (color-alpha c))]
     [([byte? _r] [byte? _g] [byte? _b])
      (set! r _r)
      (set! g _g)
      (set! b _b)]
     [([byte? _r] [byte? _g] [byte? _b] [(real-in 0 1) _a])
      (set! r _r)
      (set! g _g)
      (set! b _b)
      (set! a (exact->inexact _a))]
     (init-name 'color%))

    (define/public (red) r)
    (define/public (green) g)
    (define/public (blue) b)
    (define/public (alpha) a)

    (define/public (set rr rg rb [ra 1.0])
      (if s-immutable?
          (error (method-name 'color% 'set) "object is immutable")
          (begin
            (set! r rr)
            (set! g rg)
            (set! b rb)
            (set! a (exact->inexact ra)))))

    (define/public (ok?) #t)
    (define/public (is-immutable?) s-immutable?)
    (define/public (set-immutable) (set! s-immutable? #t))

    (define/public (copy-from c)
      (if s-immutable?
          (error (method-name 'color% 'copy-from) "object is immutable")
          (begin (set (color-red c) (color-green c) (color-blue c) (color-alpha c))
                 this)))))

(define color-red (class-field-accessor color% r))
(define color-green (class-field-accessor color% g))
(define color-blue (class-field-accessor color% b))
(define color-alpha (class-field-accessor color% a))
(define color-is-immutable? (class-field-accessor color% s-immutable?))

;; byte byte byte real -> color%
;; produce an immutable color% object
(define (make-color r g b [a 1.0])
  (define color (make-object color% r g b a))
  (send color set-immutable)
  color)

(define (color->immutable-color c)
  (if (color-is-immutable? c)
      c
      (let ([c2 (new color%)])
        (send c2 copy-from c)
        (send c2 set-immutable)
        c2)))

(define color-objects (make-hash))

(define color-database<%>
  (interface ()
    [find-color (->m string? (or/c (is-a?/c color%) #f))]
    [get-names (->m (listof string?))]))

(define color-database%
  (class* object% (color-database<%>)
    (super-new)
    (define/public (find-color name)
      (let ([name (string-downcase name)])
        (or (atomically (hash-ref color-objects name #f))
            (let ([v (hash-ref colors (string-foldcase name) #f)])
              (if v
                  (let ([c (new color%)])
                    (send c set (vector-ref v 0) (vector-ref v 1) (vector-ref v 2))
                    (send c set-immutable)
                    (atomically (hash-set! color-objects name c))
                    c)
                  #f)))))
    (define/public (get-names)
      (sort (hash-map colors (lambda (k v) k)) string<?))))

(define the-color-database (new color-database%))


;; -------------------------------------

;; (define-colors colors-id
;;   entry
;;   ...)
;;      entry = [name-group rgb-vector]
;; name-group = string    ; literal string
;;            | id        ; refer to a defined goup name
;;            | (or name-group ...)
;;            | (seq name-group ...)
;; rgb-vector = #(byte byte byte)
;; Simple example:
;; (define-colors colors
;;   ["black" #(0  0  0)]
;;   ["blue" #(0  0  255)]
;;   [(seq "dark " (or "gray" "grey")) #(169  169  169)]

;; Predefined name-groups:
;; gray, both spellings
(define-syntax gray '(or "gray" "grey"))
;; maybe-space
(define-syntax msp '(or " " ""))

(begin-for-syntax
  (define (stx-e* v)
    (cond [(syntax? v)
           (define lst (syntax->list v))
           (cond [(pair? lst) (cons (syntax-e (car lst)) (cdr lst))]
                 [lst         lst]
                 [else        (syntax-e v)])]
          [else v]))

  ;; NameGroup -> [Listof StxString]
  (define (name-group->list ng)
    (match (stx-e* ng)
      [(? symbol?)         (name-group->list (syntax-local-value ng))]
      [(? string? s)       (list s)]
      [`(or)               (list)]
      [`(or ,ng)           (name-group->list ng)]
      [`(or ,ng ...)       (append-map name-group->list ng)]
      [`(seq)              (list "")]
      [`(seq ,ng)          (name-group->list ng)]
      [`(seq ,fst ,rst ...)
       (for*/list ([fst-name (in-list (name-group->list fst))]
                   [rst-name (in-list (name-group->list `(seq ,@rst)))])
         (string-append fst-name rst-name))])))

(define-syntax define-colors
  (lambda (stx)
    (syntax-case stx []
      [(_ colors-id entry ...)
       (local [;; Entry [Hashof String RgbVector] -> [Hashof String RgbVector]
               (define (add-entry ent hsh)
                 (syntax-case ent []
                   [[name-group rgb-vector]
                    (for/fold ([hsh hsh])
                              ([name (in-list (name-group->list #'name-group))])
                      (hash-set hsh name #'rgb-vector))]))
               ;; Entries -> [Hashof String RgbVector]
               (define (entries->hash ents)
                 (foldl add-entry (hash) (syntax->list ents)))]

         #`(define colors-id
             '#,(entries->hash #'(entry ...))))])))

;; ------------------------------------
;; The Colors

(define-colors colors
  ["aliceblue"  #(240 248 255)]
  ["antiquewhite"  #(250 235 215)]
  ["aqua"  #(0 255 255)]
  ["azure"  #(240 255 255)]
  ["beige"  #(245 245 220)]
  ["bisque"  #(255 228 196)]
  ["blanchedalmond"  #(255 235 205)]
  [(seq "blue" msp "violet")  #(138 43 226)]
  ["burlywood"  #(222 184 135)]
  ; TODO: are these two versions of cadet blue supposed to be different?
  ["cadetblue"  #(95 158 160)]
  ["cadet blue"  #(96  160  160)]
  ["chartreuse"  #(127 255 0)]
  ["chocolate"  #(210 105 30)]
  ["cornflowerblue"  #(100 149 237)]
  ; TODO: is this 68 64 108 version of cornflower blue a mistake?
  ["cornflower blue"  #(68  64  108)]
  ["cornsilk"  #(255 248 220)]
  ["crimson"  #(220 20 60)]

  [(seq "dark" msp "blue")  #(0 0 139)]
  [(seq "dark" msp "cyan")  #(0 139 139)]
  [(seq "dark" msp "goldenrod")  #(184 134 11)]
  [(seq "dark" msp gray)  #(169 169 169)]
  [(seq "dark" msp "green")  #(0 100 0)]
  [(seq "dark" msp "khaki")  #(189 183 107)]
  [(seq "dark" msp "magenta")  #(139 0 139)]
  [(seq "dark" msp "olive" msp "green")  #(85 107 47)]
  [(seq "dark" msp "orange")  #(255 140 0)]
  [(seq "dark" msp "orchid")  #(153 50 204)]
  [(seq "dark" msp "red")  #(139 0 0)]
  [(seq "dark" msp "salmon")  #(233 150 122)]
  [(seq "dark" msp "sea" msp "green")  #(143 188 139)]
  [(seq "dark" msp "slate" msp "blue")  #(72 61 139)]
  [(seq "dark" msp "slate" msp gray)  #(47 79 79)]
  [(seq "dark" msp "turquoise")  #(0 206 209)]
  [(seq "dark" msp "violet")  #(148 0 211)]

  [(seq "deep" msp "pink")  #(255 20 147)]
  [(seq "deep" msp "sky" msp "blue")  #(0 191 255)]

  [(seq "dim" msp gray)  #(105 105 105)]

  ["dodgerblue"  #(30 144 255)]
  ["floralwhite"  #(255 250 240)]
  [(seq "forest" msp "green")  #(34 139 34)]
  ["fuchsia"  #(255 0 255)]
  ["gainsboro"  #(220 220 220)]
  ["ghostwhite"  #(248 248 255)]
  [(seq "green" msp "yellow")  #(173 255 47)]
  ["honeydew"  #(240 255 240)]
  ["hotpink"  #(255 105 180)]
  [(seq "indian" msp "red")  #(205 92 92)]
  ["indigo"  #(75 0 130)]
  ["ivory"  #(255 255 240)]
  ["lavender"  #(230 230 250)]
  ["lavenderblush"  #(255 240 245)]
  ["lawngreen"  #(124 252 0)]
  ["lemonchiffon"  #(255 250 205)]

  [(seq "light" msp "blue")  #(173 216 230)]
  [(seq "light" msp "coral")  #(240 128 128)]
  [(seq "light" msp "cyan")  #(224 255 255)]
  [(seq "light" msp "goldenrod" msp "yellow")  #(250 250 210)]
  [(seq "light" msp "green")  #(144 238 144)]
  [(seq "light" msp gray)  #(211 211 211)]
  [(seq "light" msp "pink")  #(255 182 193)]
  [(seq "light" msp "salmon")  #(255 160 122)]
  [(seq "light" msp "sea" msp "green")  #(32 178 170)]
  [(seq "light" msp "sky" msp "blue")  #(135 206 250)]
  [(seq "light" msp "slate" msp gray)  #(119 136 153)]
  [(seq "light" msp "steel" msp "blue")  #(176 196 222)]
  [(seq "light" msp "yellow")  #(255 255 224)]

  ["lime"  #(0 255 0)]
  [(seq "lime" msp "green")  #(50 205 50)]
  ["linen"  #(250 240 230)]

  [(seq "medium" msp "aquamarine")  #(102  205  170)]
  [(seq "medium" msp "blue")  #(0  0  205)]
  [(seq "medium" msp "forest" msp "green")  #(107  142  35)]
  [(seq "medium" msp "goldenrod")  #(234  234  173)]
  [(seq "medium" msp "orchid")  #(186  85  211)]
  [(seq "medium" msp "purple")  #(147 112 219)]
  [(seq "medium" msp "sea" msp "green")  #(60  179  113)]
  [(seq "medium" msp "slate" msp "blue")  #(123  104  238)]
  [(seq "medium" msp "spring" msp "green")  #(0  250  154)]
  [(seq "medium" msp "turquoise")  #(72  209  204)]
  [(seq "medium" msp "violet" msp "red")  #(199  21  133)]
  [(seq "midnight" msp "blue")  #(25  25  112)]
  
  ["mintcream"  #(245 255 250)]
  ["mistyrose"  #(255 228 225)]
  ["moccasin"  #(255 228 181)]
  ["navajowhite"  #(255 222 173)]
  ["oldlace"  #(253 245 230)]
  ["olive"  #(128 128 0)]
  ["olivedrab"  #(107 142 35)]
  [(seq "orange" msp "red")  #(255 69 0)]
  [(seq "pale" msp "goldenrod")  #(238 232 170)]
  [(seq "pale" msp "green")  #(152 251 152)]
  [(seq "pale" msp "turquoise")  #(175 238 238)]
  [(seq "pale" msp "violet" msp "red")  #(219 112 147)]
  ["papayawhip"  #(255 239 213)]
  ["peachpuff"  #(255 218 185)]
  ["peru"  #(205 133 63)]
  ["powderblue"  #(176 224 230)]
  ["rosybrown"  #(188 143 143)]
  ["royalblue"  #(65 105 225)]
  ["saddlebrown"  #(139 69 19)]
  ["sandybrown"  #(244 164 96)]
  [(seq "sea" msp "green")  #(46 139 87)]
  ["seashell"  #(255 245 238)]
  ["silver"  #(192 192 192)]
  [(seq "sky" msp "blue")  #(135 206 235)]

  [(seq "slate" msp "blue")  #(106 90 205)]
  [(seq "slate" msp gray)  #(112 128 144)]
  ["snow"  #(255 250 250)]
  [(seq "spring" msp "green")  #(0 255 127)]
  [(seq "steel" msp "blue")  #(70 130 180)]
  ["teal"  #(0 128 128)]
  ["tomato"  #(255 99 71)]
  ["whitesmoke"  #(245 245 245)]
  [(seq "yellow" msp "green")  #(154 205 50)]
        
  ["aquamarine"  #(112  216  144)]
  ["black"  #(0  0  0)]
  ["blue"  #(0  0  255)]
  ["brown"  #(132  60  36)]
  ["coral"  #(255  127  80)]
  ["cyan"  #(0  255  255)]
  ["firebrick"  #(178  34  34)]
  ["gold"  #(255  215  0)]
  ["goldenrod"  #(218  165  32)]
  [gray  #(190  190  190)]
  ["green"  #(0  255  0)]
  ["khaki"  #(240  230  140)]
  ["magenta"  #(255  0  255)]
  ["maroon"  #(176  48  96)]
  ["navy"  #(36  36  140)]
  ["orange"  #(255  165  0)]
  ["orchid"  #(218  112  214)]
  ["pink"  #(255  192  203)]
  ["plum"  #(221  160  221)]
  ["purple"  #(160  32  240)]
  ["red"  #(255  0  0)]
  ["salmon"  #(250  128  114)]
  ["sienna"  #(160  82  45)]
  ["tan"  #(210  180  140)]
  ["thistle"  #(216  191  216)]
  ["turquoise"  #(64  224  208)]
  ["violet"  #(238  130  238)]
  [(seq "violet" msp "red")  #(208  32  144)]
  ["wheat"  #(245  222  179)]
  ["white"  #(255  255  255)]
  ["yellow"  #(255  255  0)])
