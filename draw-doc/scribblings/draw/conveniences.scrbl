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
                     [#:arrow-root-radius arrow-root-radius real? 2.5]
                     [#:%age %age #f (or/c #f "leftup" (real-in -1 1))]
                     [#:bb bb (or/c (list/c (or/c #f real?)
                                            (or/c #f real?)
                                            (or/c #f real?)
                                            (or/c #f real?))
                                    #f)])
           void?]{
Draws an arrow on @racket[dc] from (@racket[start-x], @racket[start-y]) to
(@racket[end-x], @racket[end-y]). (@racket[dx], @racket[dy]) is the top-left
location for drawing.
If @racket[pen-width] is @racket[#f], the current pen width is used.

 If @racket[%age] is not @racket[#f], then the arrows are
 drawn with a curve. When @racket[%age] is @racket["leftup"],
 then the arrows will curve leftwards and upwards from the
 end point to the start point. When @racket[%age] is a
 number, the number controls the angle coming out of the
 starting point. If the number is negative, the arrows comes
 out to the left and if it is positive, it comes out to the
 right. The precise angle depends on the absolute value of
 the number. If the absolute value is close to @racket[1],
 the angle is close to straight out to the sides. If the
 absolute value is close to @racket[0], the angle is close to
 straight down or straight down.

 When the arrow curves (because @racket[%age] is not
 @racket[#f]) and @racket[bb] is not @racket[#f], it
 specifies a bounding box that can cause the arrow not to
 curve. Specifically, if the curve would go outside the
 bounding box, then the attempt to curve is abandoned, and a
 straight line is drawn.

@history[#:added "1.9"]{}
}
