module HSV where

import Color (..)

hsv : Float -> Float -> Float -> Color
hsv h s v = let
        hint = floor (h/60)
        hi = hint `mod` 6
        f = (h/60) - toFloat hint
        p = floor <| v*(1-s)
        q = floor <| v*(1-f*s)
        t = floor <| v*(1-(1-f)*s)
        vi = floor v
     in case hi of
        0 -> rgb vi t p
        1 -> rgb q vi p
        2 -> rgb p vi t
        3 -> rgb p q vi
        4 -> rgb t p vi
        5 -> rgb vi p q
