#lang scribble/doc
@(require "common.rkt"
          racket/draw)

@(define color-db-eval (make-base-eval))
@(interaction-eval
  #:eval color-db-eval
  (require racket/draw pict racket/class))

@definterface/title[color-database<%> ()]{

The global @indexed-racket[the-color-database] object is an instance of
 @racket[color-database<%>]. It maintains a database of standard RGB
 colors for a predefined set of named colors (such as ``black'' and
 ``light gray'').

The following colors are in the database:
 @(colors
   "Orange Red"
   "Tomato"
   "Dark Red"
   "Red"
   "Firebrick"
   "Crimson"
   "Deep Pink"
   "Maroon"
   "Indian Red"
   "Medium Violet Red"
   "Violet Red"
   "Light Coral"
   "Hot Pink"
   "Pale Violet Red"
   "Light Pink"
   "Rosy Brown"
   "Pink"
   "Orchid"
   "Lavender Blush"
   "Snow"
   "Chocolate"
   "Saddle Brown"
   "Brown"
   "Dark Orange"
   "Coral"
   "Sienna"
   "Orange"
   "Salmon"
   "Peru"
   "Dark Goldenrod"
   "Goldenrod"
   "Sandy Brown"
   "Light Salmon"
   "Dark Salmon"
   "Gold"
   "Yellow"
   "Olive"
   "Burlywood"
   "Tan"
   "Navajo White"
   "Peach Puff"
   "Khaki"
   "Dark Khaki"
   "Moccasin"
   "Wheat"
   "Bisque"
   "Pale Goldenrod"
   "Blanched Almond"
   "Medium Goldenrod"
   "Papaya Whip"
   "Misty Rose"
   "Lemon Chiffon"
   "Antique White"
   "Cornsilk"
   "Light Goldenrod Yellow"
   "Old Lace"
   "Linen"
   "Light Yellow"
   "Sea Shell"
   "Beige"
   "Floral White"
   "Ivory"
   "Green"
   "Lawn Green"
   "Chartreuse"
   "Green Yellow"
   "Yellow Green"
   "Medium Forest Green"
   "Olive Drab"
   "Dark Olive Green"
   "Dark Sea Green"
   "Lime"
   "Dark Green"
   "Lime Green"
   "Forest Green"
   "Spring Green"
   "Medium Spring Green"
   "Sea Green"
   "Medium Sea Green"
   "Aquamarine"
   "Light Green"
   "Pale Green"
   "Medium Aquamarine"
   "Turquoise"
   "Light Sea Green"
   "Medium Turquoise"
   "Honeydew"
   "Mint Cream"
   "Royal Blue"
   "Dodger Blue"
   "Deep Sky Blue"
   "CornflowerBlue"
   "Steel Blue"
   "Light Sky Blue"
   "Dark Turquoise"
   "Cyan"
   "Aqua"
   "Dark Cyan"
   "Teal"
   "Sky Blue"
   "Cadet Blue"
   "CadetBlue"
   "Dark Slate Gray"
   "Light Slate Gray"
   "Slate Gray"
   "Light Steel Blue"
   "Light Blue"
   "Powder Blue"
   "Pale Turquoise"
   "Light Cyan"
   "Alice Blue"
   "Azure"
   "Medium Blue"
   "Cornflower Blue"
   "Dark Blue"
   "Midnight Blue"
   "Navy"
   "Blue"
   "Indigo"
   "Blue Violet"
   "Medium Slate Blue"
   "Slate Blue"
   "Purple"
   "Dark Slate Blue"
   "Dark Violet"
   "Dark Orchid"
   "Medium Purple"
   "Cornflower Blue"
   "Medium Orchid"
   "Magenta"
   "Fuchsia"
   "Dark Magenta"
   "Violet"
   "Plum"
   "Lavender"
   "Thistle"
   "Ghost White"
   "White"
   "White Smoke"
   "Gainsboro"
   "Light Gray"
   "Silver"
   "Gray"
   "Dark Gray"
   "Dim Gray"
   "Black")

See also @racket[color%].


@defmethod[(find-color [color-name string?])
           (or/c (is-a?/c color%) #f)]{

  Finds a color by name (character case is ignored). If no
  color is found for the name, @racket[#f] is returned,
  otherwise the result is an immutable color object.

  Color names are normalized by case and spaces are removed
  from colors before they are looked up in the database, with
  two exceptions: @racket["cornflower blue"] and
  @racket["cadet blue"]. For those two colors, the names are
  compared in a case-insensitive way, but spaces are not
  removed, as the spaceless versions of those names are
  different colors than the ones with spaces in them.

  @examples[#:eval color-db-eval
            (define (show-colors-named . names)
              (apply
               hc-append
               2
               (for/list ([name (in-list names)])
                 (colorize
                  (filled-rectangle 60 40)
                  (send the-color-database find-color name)))))

            (show-colors-named "blue"
                               "BLUE"
                               "B L U E")

            (show-colors-named "cornflowerblue"
                               "CORNFLOWERBLUE"
                               "CORN flow ERB lue"
                               "cornflower blue"
                               "CORNFLOWER BLUE"
                               "cornflower blue "
                               " CORNFLOWER BLUE"
                               "cornflower  blue")]
}


@defmethod[(get-names) (listof string?)]{

Returns an alphabetically sorted list of case-folded color names for which
@method[color-database<%> find-color] returns a @racket[color%] value.}

}

@(close-eval color-db-eval)
