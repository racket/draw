#lang racket/base

;; Contracts for racket/draw

(require racket/contract/base
         racket/class
         "color.rkt"
         "point.rkt"
         "font.rkt"
         "font-dir.rkt"
         "font-syms.rkt"
         "pen.rkt"
         "brush.rkt"
         "gradient.rkt"
         "region.rkt"
         "bitmap.rkt"
         "dc-path.rkt"
         "dc-intf.rkt"
         "bitmap-dc.rkt"
         "post-script-dc.rkt"
         "ps-setup.rkt"
         "svg-dc.rkt"
         "gl-config.rkt"
         "gl-context.rkt")

(provide brush-style/c
         pen-cap-style/c
         pen-join-style/c
         pen-style/c
         font-family/c
         font-weight/c
         font-style/c
         transformation-vector/c
         make-color/c
         make-brush/c
         make-pen/c
         color%/c
         color-database/c
         point%/c
         font%/c
         pen%/c
         pen-list%/c
         brush%/c
         brush-list%/c
         linear-gradient%/c
         radial-gradient%/c
         bitmap-dc%/c
         post-script-dc%/c
         pdf-dc%/c
         svg-dc%/c
         record-dc%/c
         region%/c
         dc-path%/c
         gl-config%/c
         bitmap%/c)

;; dummy values to avoid cycles
(define-values (frame% dialog%) (values object% object%))

(define brush-style/c
  (or/c 'transparent 'solid 'opaque
        'xor 'hilite 'panel
        'bdiagonal-hatch 'crossdiag-hatch
        'fdiagonal-hatch 'cross-hatch
        'horizontal-hatch 'vertical-hatch))

(define pen-cap-style/c
  (or/c 'round 'projecting 'butt))

(define pen-join-style/c
  (or/c 'round 'bevel 'miter))

(define pen-style/c
  (or/c 'transparent 'solid 'xor 'hilite
        'dot 'long-dash 'short-dash 'dot-dash
        'xor-dot 'xor-long-dash 'xor-short-dash
        'xor-dot-dash))

(define transformation-vector/c
  (vector/c (vector/c real? real? real? real? real? real?)
            real? real? real? real? real?))

(define instanceof-color%/c (instanceof/c (recursive-contract color%/c)))
(define instanceof-bitmap%/c (instanceof/c (recursive-contract bitmap%/c)))
(define instanceof-linear-gradient%/c (instanceof/c (recursive-contract linear-gradient%/c)))
(define instanceof-radial-gradient%/c (instanceof/c (recursive-contract radial-gradient%/c)))
(define instanceof-brush%/c (instanceof/c (recursive-contract brush%/c)))
(define instanceof-pen%/c (instanceof/c (recursive-contract pen%/c)))
(define instanceof-region%/c (instanceof/c (recursive-contract region%/c)))
(define instanceof-dc-path%/c (instanceof/c (recursive-contract dc-path%/c)))
(define instanceof-point%/c (instanceof/c (recursive-contract point%/c)))
(define instanceof-font%/c (instanceof/c (recursive-contract font%/c)))
(define instanceof-dc<%>/c (instanceof/c (recursive-contract dc<%>/c)))

(define make-color/c
  (->* (byte? byte? byte?)
       ((real-in 0 1))
       instanceof-color%/c))

(define make-brush/c
  (->* ()
       (#:color (or/c string? instanceof-color%/c)
        #:style brush-style/c
        #:stipple (or/c #f instanceof-bitmap%/c)
        #:gradient (or/c #f
                         instanceof-linear-gradient%/c
                         instanceof-radial-gradient%/c)
        #:transformation (or/c #f transformation-vector/c)
        #:immutable? any/c)
       instanceof-brush%/c))

(define make-pen/c
  (->* ()
       (#:color (or/c string? instanceof-color%/c)
        #:width (real-in 0 255)
        #:style pen-style/c
        #:cap pen-cap-style/c
        #:join pen-join-style/c
        #:stipple (or/c #f instanceof-bitmap%/c)
        #:immutable? any/c)
       instanceof-pen%/c))

(define (mutable-color? c)
  (and (is-a? c color%)
       (not (send c is-immutable?))))

(define color%/c
  (class/c
    (alpha (->m (real-in 0 1)))
    (red  (->m byte?))
    (blue (->m byte?))
    (green (->m byte?))
    (copy-from (-> mutable-color? instanceof-color%/c instanceof-color%/c))
    (ok? (->m boolean?))
    (set (->* (mutable-color? byte? byte? byte?)
              ((real-in 0 1))
              void?))))

(define color-database/c
  (object/c
   [find-color (->m string? (or/c (instanceof/c color%/c) #f))]
   [get-names (->m (listof string?))]))

(define point%/c
  (class/c
    (get-x (->m real?))
    (get-y (->m real?))
    (set-x (->m real? void?))
    (set-y (->m real? void?))))

(define font%/c
  (class/c
    (get-face (->m (or/c string? #f)))
    (get-family (->m font-family/c))
    (get-feature-settings (->m font-feature-settings/c))
    (get-font-id (->m exact-integer?))
    (get-hinting (->m font-hinting/c))
    (get-point-size (->m (integer-in 1 1024)))
    (get-size (->*m [] [any/c] (real-in 0.0 1024.0)))
    (get-size-in-pixels (->m boolean?))
    (get-smoothing (->m font-smoothing/c))
    (get-style (->m font-style/c))
    (get-underlined (->m boolean?))
    (get-weight (->m font-weight/c))
    (screen-glyph-exists? (->*m (char?) (any/c) boolean?))))

(define pen%/c
  (class/c
    (get-cap (->m pen-cap-style/c))
    (get-color (->m (instanceof/c color%/c)))
    (get-join (->m pen-join-style/c))
    (get-stipple (->m (or/c instanceof-bitmap%/c #f)))
    (get-style (->m pen-style/c))
    (get-width (->m (real-in 0 255)))
    (set-cap (->m pen-cap-style/c void?))
    (set-color (case->m
                 (-> (or/c instanceof-color%/c string?) void?)
                 (-> byte? byte? byte? void?)))
    (set-join (->m pen-join-style/c void?))
    (set-stipple (->m (or/c instanceof-bitmap%/c #f) void?))
    (set-style (->m pen-style/c void?))
    (set-width (->m (real-in 0 255) void?))))

(define pen-list%/c
  (class/c
    (find-or-create-pen
      (->*m ((or/c instanceof-color%/c string?)
             real?
             pen-style/c)
            (pen-cap-style/c
              pen-join-style/c)
            (or/c (instanceof/c pen%/c) #f)))))

(define brush%/c
  (class/c
    (get-color (->m (instanceof/c color%/c)))
    (get-stipple (->m (or/c instanceof-bitmap%/c #f)))
    (get-style (->m brush-style/c))
    (set-color (case->m
                 (-> (or/c instanceof-color%/c string?) void?)
                 (-> byte? byte? byte? void?)))
    (set-stipple (->*m ((or/c instanceof-bitmap%/c #f))
                       ((or/c transformation-vector/c #f))
                       void?))
    (set-style (->m brush-style/c void?))))

(define brush-list%/c
  (class/c
    (find-or-create-brush
      (->m (or/c instanceof-color%/c string?)
           brush-style/c
           (or/c (instanceof/c brush%/c) #f)))))

(define linear-gradient%/c
  (class/c
    (init
      [x0 real?]
      [y0 real?]
      [x1 real?]
      [y1 real?]
      [stops (listof (list/c real? instanceof-color%/c))])
    [get-line (->m (values real? real? real? real?))]
    [get-stops (->m (listof (list/c real? instanceof-color%/c)))]))

(define radial-gradient%/c
  (class/c
    (init
      [x0 real?]
      [y0 real?]
      [r0 real?]
      [x1 real?]
      [y1 real?]
      [r1 real?]
      [stops (listof (list/c real? instanceof-color%/c))])
    [get-circles (->m (values real? real? real? real? real? real?))]
    [get-stops (->m (listof (list/c real? instanceof-color%/c)))]))

(define dc<%>/c
  (class/c
   [cache-font-metrics-key (->m exact-integer?)]
   [clear (->m void?)]
   [copy (->m real? real?
              (and/c real? (not/c negative?))
              (and/c real? (not/c negative?))
              real? real?
              void?)]
   [draw-arc (->m real? real?
                  (and/c real? (not/c negative?))
                  (and/c real? (not/c negative?))
                  real? real?
                  void?)]
   [draw-bitmap (->*m (instanceof-bitmap%/c
                       real?
                       real?)
                      ((or/c 'solid 'opaque 'xor)
                       instanceof-color%/c
                       (or/c instanceof-bitmap%/c #f))
                      boolean?)]
   [draw-bitmap-section (->*m (instanceof-bitmap%/c
                               real? real?
                               real? real?
                               (and/c real? (not/c negative?))
                               (and/c real? (not/c negative?)))
                              ((or/c 'solid 'opaque 'xor)
                               instanceof-color%/c
                               (or/c instanceof-bitmap%/c #f))
                              boolean?)]
   [draw-ellipse (->m real? real?
                      (and/c real? (not/c negative?))
                      (and/c real? (not/c negative?))
                      void?)]
   [draw-line (->m real? real? real? real? void?)]
   [draw-lines (->*m ((or/c (listof instanceof-point%/c)
                            (listof (cons/c real? real?))))
                     (real?
                      real?)
                     void?)]
   [draw-path (->*m (instanceof-dc-path%/c)
                    (real?
                     real?
                     (or/c 'odd-even 'winding))
                    void?)]
   [draw-point (->m real? real? void?)]
   [draw-polygon (->*m ((or/c (listof instanceof-point%/c)
                              (listof (cons/c real? real?))))
                       (real?
                        real?
                        (or/c 'odd-even 'winding))
                       void?)]
   [draw-rectangle (->m real? real?
                        (and/c real? (not/c negative?))
                        (and/c real? (not/c negative?))
                        void?)]
   [draw-rounded-rectangle (->*m (real? real?
                                        (and/c real? (not/c negative?))
                                        (and/c real? (not/c negative?)))
                                 (real?)
                                 void?)]
   [draw-spline (->m real? real?
                     real? real?
                     real? real?
                     void?)]
   [draw-text (->*m (string?
                     real? real?)
                    (any/c
                     exact-nonnegative-integer?
                     real?)
                    void?)]
   [end-alpha (->m void?)]
   [end-doc (->m void?)]
   [end-page (->m void?)]
   [erase (->m void?)]
   [flush (->m void?)]
   [get-alpha (->m (real-in 0 1))]
   [get-background (->m instanceof-color%/c)]
   [get-backing-scale (->m (>/c 0.0))]
   [get-brush (->m instanceof-brush%/c)]
   [get-char-height (->m (and/c real? (not/c negative?)))]
   [get-char-width (->m (and/c real? (not/c negative?)))]
   [get-clipping-region (->m (or/c instanceof-region%/c #f))]
   [get-device-scale (->m (values (and/c real? (not/c negative?))
                                  (and/c real? (not/c negative?))))]
   [get-font (->m instanceof-font%/c)]
   [get-gl-context (->m (or/c (is-a?/c gl-context<%>) #f))]
   [get-initial-matrix (->m (vector/c real? real? real? real? real? real?))]
   [get-origin (->m (values real? real?))]
   [get-pen (->m instanceof-pen%/c)]
   [get-path-bounding-box (->m instanceof-dc-path%/c
                               (or/c 'path 'stroke 'fill)
                               (values real? real? real? real?))]
   [get-rotation (->m real?)]
   [get-scale (->m (values real? real?))]
   [get-size (->m (values (and/c real? (not/c negative?))
                          (and/c real? (not/c negative?))))]
   [get-smoothing (->m (or/c 'unsmoothed 'smoothed 'aligned))]
   [get-text-background (->m instanceof-color%/c)]
   [get-text-extent (->*m (string?)
                          ((or/c instanceof-font%/c #f)
                           any/c
                           exact-nonnegative-integer?)
                          (values (and/c real? (not/c negative?))
                                  (and/c real? (not/c negative?))
                                  (and/c real? (not/c negative?))
                                  (and/c real? (not/c negative?))))]
   [get-text-foreground (->m instanceof-color%/c)]
   [get-text-mode (->m (or/c 'solid 'transparent))]
   [get-transformation (->m (vector/c (vector/c real? real? real? real? real? real?)
                                      real? real? real? real? real?))]
   [glyph-exists? (->m char? boolean?)]
   [ok? (->m boolean?)]
   [resume-flush (->m void?)]
   [rotate (->m real? void?)]
   [scale (->m real? real? void?)]
   [set-alignment-scale (->m (>/c 0.0) void?)]
   [set-alpha (->m (real-in 0 1) void?)]
   [set-background (->m (or/c instanceof-color%/c string?) void?)]
   [set-brush (case->
               (-> any/c instanceof-brush%/c void?)
               (-> any/c
                   (or/c instanceof-color%/c string?)
                   (or/c 'transparent 'solid 'opaque
                         'xor 'hilite 'panel
                         'bdiagonal-hatch 'crossdiag-hatch
                         'fdiagonal-hatch 'cross-hatch
                         'horizontal-hatch 'vertical-hatch)
                   void?))]
   [set-clipping-rect (->m real? real?
                           (and/c real? (not/c negative?))
                           (and/c real? (not/c negative?))
                           void?)]
   [set-clipping-region (->m (or/c instanceof-region%/c #f) void?)]
   [set-font (->m instanceof-font%/c void?)]
   [set-initial-matrix (->m (vector/c real? real? real? real? real? real?) void?)]
   [set-origin (->m real? real? void?)]
   [set-pen (case->
             (-> any/c instanceof-pen%/c void?)
             (-> any/c
                 (or/c instanceof-color%/c string?)
                 (real-in 0 255)
                 (or/c 'transparent 'solid 'xor 'hilite
                       'dot 'long-dash 'short-dash 'dot-dash
                       'xor-dot 'xor-long-dash 'xor-short-dash
                       'xor-dot-dash)
                 void?))]
   [set-rotation (->m real? void?)]
   [set-scale (->m real? real? void?)]
   [set-smoothing (->m (or/c 'unsmoothed 'smoothed 'aligned) void?)]
   [set-text-background (->m (or/c instanceof-color%/c string?) void?)]
   [set-text-foreground (->m (or/c instanceof-color%/c string?) void?)]
   [set-text-mode (->m (or/c 'solid 'transparent) void?)]
   [set-transformation (->m (vector/c (vector/c real? real? real? real? real? real?)
                                      real? real? real? real? real?)
                             void?)]
   [start-alpha (->m (real-in 0 1) void?)]
   [start-doc (->m string? void?)]
   [start-page (->m void?)]
   [suspend-flush (->m void?)]
   [transform (->m (vector/c real? real? real? real? real? real?)
                   void?)]
   [translate (->m real? real? void?)]
   [try-color (->m instanceof-color%/c instanceof-color%/c void?)]))

(define bitmap-dc%/c
  (and/c
   dc<%>/c
   (class/c
    (init [bitmap (or/c instanceof-bitmap%/c #f)])
    [draw-bitmap-section-smooth
     (->*m (instanceof-bitmap%/c
            real? real?
            (and/c real? (not/c negative?))
            (and/c real? (not/c negative?))
            real? real?
            (and/c real? (not/c negative?))
            (and/c real? (not/c negative?)))
           ((or/c 'solid 'opaque 'xor)
            (or/c instanceof-color%/c #f)
            (or/c instanceof-bitmap%/c #f))
           boolean?)]
    [get-argb-pixels
     (->*m (exact-nonnegative-integer?
            exact-nonnegative-integer?
            exact-nonnegative-integer?
            exact-nonnegative-integer?
            (and/c bytes? (not/c immutable?)))
           (any/c any/c)
           void?)]
    [get-bitmap (->m (or/c instanceof-bitmap%/c #f))]
    [get-pixel (->m exact-nonnegative-integer? exact-nonnegative-integer? instanceof-color%/c boolean?)]
    [set-argb-pixels
     (->*m (exact-nonnegative-integer?
            exact-nonnegative-integer?
            exact-nonnegative-integer?
            exact-nonnegative-integer?
            bytes?)
           (any/c any/c)
           void?)]
    [set-bitmap (->m (or/c instanceof-bitmap%/c #f) void?)]
    [set-pixel (->m real? real? instanceof-color%/c void?)])))

(define post-script-dc%/c
  (and/c
   dc<%>/c
   (class/c
    (init [interactive any/c]
          [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) #f)]
          [use-paper-bbox any/c]
          [as-eps any/c]
          [width (or/c (and/c real? (not/c negative?)) #f)]
          [height (or/c (and/c real? (not/c negative?)) #f)]
          [output (or/c path-string? output-port? #f)]))))

(define pdf-dc%/c
  (and/c
   dc<%>/c
   (class/c
    (init [interactive any/c]
          [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) #f)]
          [use-paper-bbox any/c]
          [as-eps any/c]
          [width (or/c (and/c real? (not/c negative?)) #f)]
          [height (or/c (and/c real? (not/c negative?)) #f)]
          [output (or/c path-string? output-port? #f)]))))

(define svg-dc%/c
  (and/c
   dc<%>/c
   (class/c
    (init [width (or/c (and/c real? (not/c negative?)) #f)]
          [height (or/c (and/c real? (not/c negative?)) #f)]
          [output (or/c path-string? output-port? #f)]
          [exists (or/c 'error 'append 'update 'can-update
                        'replace 'truncate
                        'must-truncate 'truncate/replace)]))))

(define region%/c
  (class/c
    (init [dc (or/c instanceof-dc<%>/c #f)])
    (get-bounding-box (->m (values real? real? real? real?)))
    (get-dc (->m (or/c instanceof-dc<%>/c #f)))
    (in-region? (->m real? real? boolean?))
    (intersect (->m instanceof-region%/c void?))
    (is-empty? (->m boolean?))
    (set-arc (->m real?
                  real?
                  (and/c real? (not/c negative?))
                  (and/c real? (not/c negative?))
                  real?
                  real?
                  void?))
    (set-ellipse (->m real?
                      real?
                      (and/c real? (not/c negative?))
                      (and/c real? (not/c negative?))
                      void?))
    (set-path (->*m (instanceof-dc-path%/c)
                    (real?
                     real?
                     (or/c 'odd-even 'winding))
                    void?))
    (set-polygon (->*m ((or/c (listof instanceof-point%/c)
                              (listof (cons/c real? real?))))
                       (real?
                        real?
                        (or/c 'odd-even 'winding))
                       void?))
    (set-rectangle (->m real?
                        real?
                        (and/c real? (not/c negative?))
                        (and/c real? (not/c negative?))
                        void?))
    (set-rounded-rectangle (->*m (real?
                                  real?
                                  (and/c real? (not/c negative?))
                                  (and/c real? (not/c negative?)))
                                 (real?)
                                 void?))
    (subtract (->m instanceof-region%/c void?))
    (union (->m instanceof-region%/c void?))
    (xor (->m instanceof-region%/c void?))))

(define record-dc%/c
  (class/c
    (init [width (>=/c 0)]
          [height (>=/c 0)]
          [record-ink? any/c])
    [get-clipping-region (->m (or/c #f (instanceof/c region%/c)))]
    [get-recorded-datum (->m any/c)]
    [get-recorded-procedure (->m ((is-a?/c dc<%>) . -> . void?))]
    [get-ink-extent (->m (values real? real? real? real?))]))

(define dc-path%/c
  (class/c
    (append (->m instanceof-dc-path%/c void?))
    (arc (->*m (real?
                real?
                real?
                real?
                real?
                real?)
               (any/c)
               void?))
    (close (->m void?))
    (curve-to (->m real? real? real? real? real? real? void?))
    (ellipse (->m real?
                  real?
                  (and/c real? (not/c negative?))
                  (and/c real? (not/c negative?))
                  void?))
    (get-bounding-box (->m (values real? real? real? real?)))
    (line-to (->m real? real? void?))
    (lines (->*m ((or/c (listof instanceof-point%/c)
                        (listof (cons/c real? real?))))
                 (real? real?)
                 void?))
    (move-to (->m real? real? void?))
    (open? (->m boolean?))
    (rectangle (->m real?
                    real?
                    (and/c real? (not/c negative?))
                    (and/c real? (not/c negative?))
                    void?))
    (reset (->m void?))
    (reverse (->m void?))
    (rotate (->m real? void?))
    (rounded-rectangle (->*m (real?
                              real?
                              (and/c real? (not/c negative?))
                              (and/c real? (not/c negative?)))
                             (real?)
                             void?))
    (scale (->m real? real? void?))
    (text-outline (->*m (instanceof-font%/c
                         string?
                         real? real?)
                        (any/c)
                        void?))
    (transform (->m (vector/c real? real? real? real? real? real?)
                    void?))
    (translate (->m real? real? void?))))

(define gl-config%/c
  (class/c
    (get-accum-size (->m (integer-in 0 256)))
    (get-depth-size (->m (integer-in 0 256)))
    (get-double-buffered (->m boolean?))
    (get-multisample-size (->m (integer-in 0 256)))
    (get-stencil-size (->m (integer-in 0 256)))
    (get-stereo (->m boolean?))
    (set-accum-size (->m (integer-in 0 256) void?))
    (set-depth-size (->m (integer-in 0 256) void?))
    (set-double-buffered (->m any/c void?))
    (set-multisample-size (->m (integer-in 0 256) void?))
    (set-stencil-size (->m (integer-in 0 256) void?))
    (set-stereo (->m any/c void?))
    (set-share-context (->m (or/c (is-a?/c gl-context%) #f) void?))
    (get-legacy? (->m boolean?))
    (set-legacy? (->m any/c void?))))

(define bitmap%/c
  (class/c
    (get-argb-pixels (->*m
                       (exact-nonnegative-integer?
                        exact-nonnegative-integer?
                        exact-nonnegative-integer?
                        exact-nonnegative-integer?
                        (and/c bytes? (not/c immutable?)))
                       (any/c any/c #:unscaled? any/c)
                       void?))
    (get-backing-scale (->m (>/c 0.0)))
    (get-depth (->m exact-nonnegative-integer?))
    (get-height (->m exact-nonnegative-integer?))
    (get-loaded-mask (->m (or/c instanceof-bitmap%/c #f)))
    (get-width (->m exact-nonnegative-integer?))
    (get-data-from-file (->m (or/c (vector/c (or/c 'unknown 'unknown/mask 'unknown/alpha
                                                   'gif 'gif/mask 'gif/alpha
                                                   'jpeg 'jpeg/alpha
                                                   'png 'png/mask 'png/alpha
                                                   'xbm 'xbm/alpha 'xpm 'xpm/alpha
                                                   'bmp 'bmp/alpha)
                                             (or/c (instanceof/c color%/c) #f) ;; actually always immutable
                                             (and/c bytes? immutable?)
                                             #:immutable #t)
                                   #f)))
    (has-alpha-channel? (->m boolean?))
    (is-color? (->m boolean?))
    (load-file (->*m ((or/c path-string? input-port?))
                     ((or/c 'unknown 'unknown/mask 'unknown/alpha
                            'gif 'gif/mask 'gif/alpha
                            'jpeg 'jpeg/alpha
                            'png 'png/mask 'png/alpha
                            'xbm 'xbm/alpha 'xpm 'xpm/alpha
                            'bmp 'bmp/alpha)
                      (or/c instanceof-color%/c #f)
                      any/c
                      #:save-data-from-file? any/c)
                     boolean?))
    (ok? (->m boolean?))
    (save-file (->*m ((or/c path-string? output-port?)
                      (or/c 'png 'jpeg 'xbm 'xpm 'bmp))
                     ((integer-in 0 100)
                      #:unscaled? any/c)
                     boolean?))
    (set-argb-pixels (->*m
                       (exact-nonnegative-integer?
                        exact-nonnegative-integer?
                        exact-nonnegative-integer?
                        exact-nonnegative-integer?
                        bytes?)
                       (any/c any/c #:unscaled? any/c)
                       void?))
    (set-loaded-mask (->m instanceof-bitmap%/c void?))))
