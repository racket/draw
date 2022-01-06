#lang scribble/doc
@(require "common.rkt")

@defclass/title[font-list% object% ()]{

A @racket[font-list%] object maintains a list of @racket[font%]
 objects to avoid repeatedly creating fonts.

A global font list, @racket[the-font-list], is created automatically.


@defconstructor[()]{

Creates an empty font list.

}

@defmethod*[([(find-or-create-font [size (real-in 0.0 1024.0)]
                                   [family (or/c 'default 'decorative 'roman 'script 
                                                 'swiss 'modern 'symbol 'system)]
                                   [style (or/c 'normal 'italic 'slant)]
                                   [weight (or/c 'normal 'bold 'light)]
                                   [underline? any/c #f]
                                   [smoothing (or/c 'default 'partly-smoothed 'smoothed 'unsmoothed) 'default]
                                   [size-in-pixels? any/c #f]
                                   [hinting (or/c 'aligned 'unaligned) 'aligned]
                                   [feature-settings font-feature-settings/c (hash)])
              (is-a?/c font%)]
             [(find-or-create-font [size (real-in 0.0 1024.0)]
                                   [face string?]
                                   [family (or/c 'default 'decorative 'roman 'script
                                                 'swiss 'modern 'symbol 'system)]
                                   [style (or/c 'normal 'italic 'slant)]
                                   [weight (or/c 'normal 'bold 'light)]
                                   [underline any/c #f]
                                   [smoothing (or/c 'default 'partly-smoothed 'smoothed 'unsmoothed) 'default]
                                   [size-in-pixels? any/c #f]
                                   [hinting (or/c 'aligned 'unaligned) 'aligned]
                                   [feature-settings font-feature-settings/c (hash)])
              (is-a?/c font%)])]{

Finds an existing font in the list or creates a new one (that is
 automatically added to the list). The arguments are the same as for
 creating a @racket[font%] instance.

See also @racket[make-font] and @racket[current-font-list].

@history[#:changed "1.4" @elem{Changed @racket[size] to allow non-integer and zero values.}
         #:changed "1.19" @elem{Added the optional @racket[feature-settings] argument.}]}}
