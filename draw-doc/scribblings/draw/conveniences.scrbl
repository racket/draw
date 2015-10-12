#lang scribble/manual

@(require "common.rkt" (for-label racket/draw/arrow))

@title{Drawing Conveniences}

This section presents higher-level APIs that provide additional conveniences
over the @racket[racket/draw] API.

@section{Arrows}
@defmodule[racket/draw/arrow]

@defproc[(draw-arrow [dc (is-a?/c dc<%>)]
                     [start-x real?]
                     [start-y real?]
                     [end-x real?]
                     [end-y real?]
                     [dx real?]
                     [dy real?]
                     [#:pen-width pen-width (or/c real? #f) #f]
                     [#:arrow-head-size arrow-head-size real? 8]
                     [#:arrow-root-radius arrow-root-radius real? 2.5])
           void?]{
Draws an arrow on @racket[dc] from (@racket[start-x], @racket[start-y]) to
(@racket[end-x], @racket[end-y]). (@racket[dx], @racket[dy]) is the top-left
location for drawing.
If @racket[pen-width] is @racket[#f], the current pen width is used.

@history[#:added "1.9"]{}
}
