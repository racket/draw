#readerscribble/reader
(module blurbs racket/base
  (require scribble/struct
           scribble/manual
           scribble/scheme
           scribble/decode
           racket/set
           racket/class
           racket/draw
           (only-in racket/draw/private/color normalize-color-name)
           (for-label racket/draw
                      racket/base)
           (for-syntax racket/base))

  (provide (all-defined-out))

  (define (p . l)
    (decode-paragraph l))

  (define PrintNote
    (make-splice
     (list
      @p{Be sure to use the following methods to start/end drawing:}
      @itemize[@item{@method[dc<%> start-doc]}
               @item{@method[dc<%> start-page]}
               @item{@method[dc<%> end-page]}
               @item{@method[dc<%> end-doc]}]
      @p{Attempts to use a drawing method outside of an active page raises an exception.})))

  (define reference-doc '(lib "scribblings/reference/reference.scrbl"))

  (define SeeMzParam @elem{(see @secref[#:doc reference-doc "parameters"])})
  
  (define DrawSizeNote "")

  (define MismatchExn @elem{an @racket[exn:fail:contract] exception is raised})

  ;; currently also used by the `2htdp/image` docs:
  (define (colorName color-name ignored r g b)
    (make-element #f
                  (list (make-element `(bg-color ,r ,g ,b)
                                      (list (hspace 5)))
                        (hspace 1)
                        (make-element 'tt (if (bytes? color-name)
                                              (bytes->string/latin-1 color-name)
                                              color-name)))))

  (define (colors . colors)
    (define all-colors
      (apply set (map normalize-color-name (send the-color-database get-names))))
    (define result
      (tabular
       (for/list ([color-name (in-list colors)])
         (define color (send the-color-database find-color color-name))
         (set! all-colors (set-remove all-colors (normalize-color-name color-name)))
         (list (colorName color-name #f (send color red) (send color green) (send color blue))))))
    (unless (set-empty? all-colors)
      (error 'colors "did not cover ~s" (sort (set->list all-colors) string<?)))
    result)


  (define (slant . s)
    (make-element "slant" (decode-content s)))

  (define (res-sym s)
    (string->symbol (string-append "GRacket:" s)))

  (define (boxisfill which what)
    @elem{The @|which| box is filled with @|what|.})
  (define (boxisfillnull which what)
    @elem{The @|which| box is filled with @|what|, unless @|which| is @racket[#f].})

  )

