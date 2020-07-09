#lang racket/base
(require racket/class
         ffi/unsafe/atomic
         "syntax.rkt"
         "../unsafe/pango.rkt"
         "../unsafe/cairo.rkt"
         "font-syms.rkt"
         "font-dir.rkt"
         "local.rkt"
         "xp.rkt"
         "lock.rkt")

(provide font%
         font-list% the-font-list
         make-font
         family-symbol? style-symbol? smoothing-symbol? hinting-symbol?
         get-pango-attrs
         get-face-list
         (protect-out substitute-fonts?
                      install-alternate-face
                      font->pango-attrs
                      font->hinting
                      install-attributes!))

(define-local-member-name 
  get-pango-attrs
  s-pango-attrs
  s-hinting)

(define fallback-attrs (and (or xp? (eq? 'macosx (system-type)))
			    (let ([l (pango_attr_list_new)])
			      (pango_attr_list_insert l (pango_attr_fallback_new #f))
			      l)))
(define underlined-attrs (let ([l (pango_attr_list_new)])
                           (pango_attr_list_insert l (pango_attr_underline_new
                                                      PANGO_UNDERLINE_SINGLE))
                           (when (eq? 'macosx (system-type))
                             (pango_attr_list_insert l (pango_attr_fallback_new #f)))
                           l))

(define always-attrs (and (eq? 'macosx (system-type)) fallback-attrs))

(define (install-attributes! layout attrs)
  (cond
   [attrs (pango_layout_set_attributes layout attrs)]
   [always-attrs (pango_layout_set_attributes layout always-attrs)]))

(define-local-member-name s-set-table-key)

(define font-descs (make-weak-hash))
(define ps-font-descs (make-weak-hash))
(define keys (make-weak-hash))

(define substitute-fonts? (memq (system-type) '(macosx windows)))
(define substitute-mapping (make-hasheq))

(define (install-alternate-face ch layout font desc attrs context)
  (or
   (for/or ([face (in-list 
                   (let ([v (hash-ref substitute-mapping (char->integer ch) #f)])
                     (cond
                      [(string? v) 
                       ;; found previously
                       (list v)]
                      [v 
                       ;; failed to find previously
                       null]
                      [else
                       ;; Hack: prefer a particular font for Mac OS
                       (cons "Arial Unicode MS" (get-face-list))])))])
     (let ([desc (send (make-object font%
                                    (send font get-point-size)
                                    face
                                    (send font get-family)
                                    (send font get-style)
                                    (send font get-weight)
                                    (send font get-underlined)
                                    (send font get-smoothing)
                                    (send font get-size-in-pixels))
                       get-pango)])
       (and desc
            (let ([attrs (send font get-pango-attrs)])
              (pango_layout_set_font_description layout desc)
              (install-attributes! layout attrs)
              (and (zero? (pango_layout_get_unknown_glyphs_count layout))
                   (begin
                     (hash-set! substitute-mapping (char->integer ch) face)
                     #t))))))
   (begin
     (hash-set! substitute-mapping (char->integer ch) #t)
     ;; put old desc & attrs back
     (pango_layout_set_font_description layout desc)
     (install-attributes! layout attrs))))

(define (has-screen-glyph? c font desc for-label?)
  (let* ([s (cairo_image_surface_create CAIRO_FORMAT_ARGB32 1 1)]
         [cr (cairo_create s)]
         [context (pango_cairo_create_context cr)]
         [layout (pango_layout_new context)]
	 ;; Under Windows XP, there's no font 
	 ;; fallback/substitution in control labels:
	 [no-subs? (and xp? for-label?)])
    (pango_layout_set_font_description layout desc)
    (pango_layout_set_text layout (string c))
    (when no-subs?
      (pango_layout_set_attributes layout fallback-attrs))
    (pango_cairo_update_layout cr layout)
    (begin0
     (or (zero? (pango_layout_get_unknown_glyphs_count layout))
         (and substitute-fonts?
	      (not no-subs?)
              (install-alternate-face c layout font desc #f context)
              (zero? (pango_layout_get_unknown_glyphs_count layout))))
     (g_object_unref layout)
     (g_object_unref context)
     (cairo_destroy cr)
     (cairo_surface_destroy s))))

(define dpi-scale
  ;; Hard-wire 96dpi for Windows and Linux, but 72 dpi for Mac OS
  ;; (based on historical defaults on those platforms).
  ;; If the actual DPI for the screen is different, we'll handle
  ;; that by scaling to and from the screen.
  (if (eq? 'macosx (system-type))
      1.0
      (/ 96.0 72.0)))

(defclass font% object%

  (define table-key #f)
  (define/public (s-set-table-key k) (set! table-key k))

  (define cached-desc #f)
  (define ps-cached-desc #f)
  
  (define/public (get-pango)
    (create-desc #f 
                 cached-desc
                 font-descs
                 (lambda (d) (set! cached-desc d))))

  (define/public (get-ps-pango)
    (create-desc #t
                 ps-cached-desc
                 ps-font-descs
                 (lambda (d) (set! ps-cached-desc d))))

  (define/private (create-desc ps? cached-desc font-descs install!)
    (or cached-desc
        (let ([desc-e (atomically (hash-ref font-descs key #f))])
          (and desc-e
               (let ([desc (ephemeron-value desc-e)])
                 (install! desc)
                 desc)))
        (let* ([desc-str (if ps?
                             (send the-font-name-directory
                                   get-post-script-name
                                   id
                                   weight
                                   style)
                             (send the-font-name-directory
                                   get-screen-name
                                   id
                                   weight
                                   style))]
               [desc (if (regexp-match #rx"," desc-str)
                         ;; comma -> a font description
                         (pango_font_description_from_string desc-str)
                         ;; no comma -> a font family
                         (let ([desc (pango_font_description_new)])
                           (pango_font_description_set_family desc desc-str)
                           desc))])
          (unless (eq? style 'normal)
            (pango_font_description_set_style desc (case style
                                                     [(normal) PANGO_STYLE_NORMAL]
                                                     [(italic) PANGO_STYLE_ITALIC]
                                                     [(slant) PANGO_STYLE_OBLIQUE])))
          (unless (or (eq? weight 'normal) (eqv? weight 300))
            (pango_font_description_set_weight desc (case weight
                                                      [(thin) PANGO_WEIGHT_THIN]
                                                      [(ultralight) PANGO_WEIGHT_ULTRALIGHT]
                                                      [(light) PANGO_WEIGHT_LIGHT]
                                                      [(semilight) PANGO_WEIGHT_SEMILIGHT]
                                                      [(book) PANGO_WEIGHT_BOOK]
                                                      [(normal) PANGO_WEIGHT_NORMAL]
                                                      [(medium) PANGO_WEIGHT_MEDIUM]
                                                      [(semibold) PANGO_WEIGHT_SEMIBOLD]
                                                      [(bold) PANGO_WEIGHT_BOLD]
                                                      [(ultrabold) PANGO_WEIGHT_ULTRABOLD]
                                                      [(heavy) PANGO_WEIGHT_HEAVY]
                                                      [(ultraheavy) PANGO_WEIGHT_ULTRAHEAVY]
                                                      [else weight])))
          (let ([size (if size-in-pixels? size (* dpi-scale size))])
            (pango_font_description_set_absolute_size desc (* size PANGO_SCALE)))
          (install! desc)
          (atomically (hash-set! font-descs key (make-ephemeron key desc)))
          desc)))

  (field [s-pango-attrs #f])
  (define/public (get-pango-attrs)
    s-pango-attrs)

  (define face #f)
  (def/public (get-face) face)

  (define family 'default)
  (def/public (get-family) family)

  (define size 12.0)
  (def/public (get-point-size) (max 1 (inexact->exact (round size))))
  
  (def/public (get-size) size)

  (define size-in-pixels? #f)
  (def/public (get-size-in-pixels) size-in-pixels?)

  (define smoothing 'default)
  (def/public (get-smoothing) smoothing)
  
  (field [s-hinting 'aligned])
  (def/public (get-hinting) s-hinting)
  
  (define style 'normal)
  (def/public (get-style) style)

  (def/public (get-underlined) (and s-pango-attrs #t))

  (define weight 'normal)
  (def/public (get-weight) weight)

  (def/public (get-font-id) id)
  (def/public (get-font-key) key)

  (define/public (screen-glyph-exists? c [for-label? #f])
    (has-screen-glyph? c this (get-pango) for-label?))

  (init-rest args)
  (super-new)
  (case-args 
   args
   [() (void)]
   [([(real-in 0.0 1024.0) _size]
     [family-symbol? _family]
     [style-symbol? [_style 'normal]]
     [font-weight/c [_weight 'normal]]
     [any? [_underlined? #f]]
     [smoothing-symbol? [_smoothing 'default]]
     [any? [_size-in-pixels? #f]]
     [hinting-symbol? [_hinting 'aligned]])
    (set! size (exact->inexact _size))
    (set! family _family)
    (set! style _style)
    (set! weight _weight)
    (set! s-pango-attrs (and _underlined? underlined-attrs))
    (set! smoothing _smoothing)
    (set! size-in-pixels? _size-in-pixels?)
    (set! s-hinting _hinting)]
   [([(real-in 0.0 1024.0) _size]
     [(make-or-false string?) _face]
     [family-symbol? _family]
     [style-symbol? [_style 'normal]]
     [font-weight/c [_weight 'normal]]
     [any? [_underlined? #f]]
     [smoothing-symbol? [_smoothing 'default]]
     [any? [_size-in-pixels? #f]]
     [hinting-symbol? [_hinting 'aligned]])
    (set! size (exact->inexact _size))
    (set! face (and _face (string->immutable-string _face)))
    (set! family _family)
    (set! style _style)
    (set! weight _weight)
    (set! s-pango-attrs (and _underlined? underlined-attrs))
    (set! smoothing _smoothing)
    (set! size-in-pixels? _size-in-pixels?)
    (set! s-hinting _hinting)]
   (init-name 'font%))

  (define id 
    (if face
        (send the-font-name-directory find-or-create-font-id face family)
        (send the-font-name-directory find-family-default-font-id family)))
  (define key
    (let ([key (vector id size style weight (and s-pango-attrs #t) smoothing size-in-pixels? s-hinting)])
      (let ([old-key (atomically (hash-ref keys key #f))])
        (if old-key
            (weak-box-value old-key)
            (begin
              (atomically (hash-set! keys key (make-weak-box key)))
              key))))))

(define font->pango-attrs (class-field-accessor font% s-pango-attrs))
(define font->hinting (class-field-accessor font% s-hinting))

;; ----------------------------------------

(defclass font-list% object%
  (define fonts (make-weak-hash))
  (super-new)
  (define/public (find-or-create-font . args)
    (let ([key
           (case-args 
            args
            [([(real-in 0.0 1024.0) size]
              [family-symbol? family]
              [style-symbol? [style 'normal]]
              [font-weight/c [weight 'normal]]
              [any? [underlined? #f]]
              [smoothing-symbol? [smoothing 'default]]
              [any? [size-in-pixels? #f]]
              [hinting-symbol? [hinting 'aligned]])
             (vector (exact->inexact size) family style weight underlined? smoothing size-in-pixels? hinting)]
            [([(real-in 0.0 1024.0) size]
              [(make-or-false string?) face]
              [family-symbol? family]
              [style-symbol? [style 'normal]]
              [font-weight/c [weight 'normal]]
              [any? [underlined? #f]]
              [smoothing-symbol? [smoothing 'default]]
              [any? [size-in-pixels? #f]]
              [hinting-symbol? [hinting 'aligned]])
             (vector (exact->inexact size) (and face (string->immutable-string face)) family
                     style weight underlined? smoothing size-in-pixels? hinting)]
            (method-name 'find-or-create-font 'font-list%))])
      (atomically
       (let ([e (hash-ref fonts key #f)])
         (or (and e
                  (ephemeron-value e))
             (let* ([f (apply make-object font% (vector->list key))]
                    [e (make-ephemeron key f)])
               (send f s-set-table-key key)
               (hash-set! fonts key e)
               f)))))))

(define the-font-list (new font-list%))

(define (get-face-list [mode 'all] #:all-variants? [all-variants? #f])
  (unless (or (eq? mode 'all) (eq? mode 'mono))
    (raise-type-error get-face-list "'all or 'mono" mode))
  (sort
   (apply
    append
    (for/list ([fam (in-list
                     (let ([fams (pango_font_map_list_families
                                  (pango_cairo_font_map_get_default))])
                       (if (eq? mode 'mono)
                           (filter pango_font_family_is_monospace fams)
                           fams)))])
      (if (not all-variants?)
          (list (pango_font_family_get_name fam))
          (for/list ([face (in-list (pango_font_family_list_faces fam))])
            (define family-name (pango_font_family_get_name fam))
            (define full-name (pango_font_description_to_string
                               (pango_font_face_describe face)))
            (define len (string-length family-name))
            ;; Normally, the full description will extend the family name:
            (cond
             [(and ((string-length full-name) . > . (+ len 1))
                   (string=? (substring full-name 0 len) family-name)
                   (char=? #\space (string-ref full-name len)))
              (string-append family-name "," (substring full-name len))]
             [#f
              ;; If the full description doesn't extend the name, then we
              ;; could show more information by adding the font's declared
              ;; face string. But that may not be parseable by Pango, so
              ;; we don't return this currently. Maybe one day add an option
              ;; to expose this string.
              (string-append family-name ", " (pango_font_face_get_face_name face))]
             [else
              ;; In this case, we can't say more than just the family name,
              ;; even though that may produce duplicates (but usually won't)
              family-name])))))
   string<?))

(define (make-font #:size [size 12.0]
                   #:face [face #f]
                   #:family [family 'default]
                   #:style [style 'normal]
                   #:weight [weight 'normal]
                   #:underlined? [underlined? #f]
                   #:smoothing [smoothing 'default]
                   #:size-in-pixels? [size-in-pixels? #f]
                   #:hinting [hinting 'aligned])
  (unless (and (real? size) (<= 0.0 size 1024.0)) (raise-type-error 'make-font "real number in [0.0, 1024.0]" size))
  (unless (or (not face) (string? face)) (raise-type-error 'make-font "string or #f" face))
  (unless (family-symbol? family) (raise-type-error 'make-font "family-symbol" family))
  (unless (style-symbol? style) (raise-type-error 'make-font "style-symbol" style))
  (unless (font-weight/c weight) (raise-argument-error 'make-font "font-weight/c" weight))
  (unless (smoothing-symbol? smoothing) (raise-type-error 'make-font "smoothing-symbol" smoothing))
  (unless (hinting-symbol? hinting) (raise-type-error 'make-font "hinting-symbol" hinting))
  (make-object font% size face family style weight underlined? smoothing size-in-pixels? hinting))
