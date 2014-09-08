module Grid where

--------------------------------------------------------------------------------
-- | Show a list of element as a grid inside a window
reactiveGrid : (Int,Int)        -- ^ size of inner grid cell
                -> [Element]    -- ^ list of elements to show
                -> Int          -- ^ width of containing box
                -> Element
reactiveGrid (boxWidth,boxHeight) elements w =
    let
        nbOnRow             = div w boxWidth
        nbLines             = length grid
        grid                = groupBy nbOnRow elements
        box                 = container boxWidth boxHeight middle
        drawOneLine oneLine = flow right <| map box oneLine
    in
        container w (boxHeight * nbLines) midTop <|
            flow down <| map drawOneLine grid

--------------------------------------------------------------------------------
-- | group element of a list by some number
-- |
-- | groupBy 3 [1,2,3,4,5,6,7] = [[1,2,3],[4,5,6],[7]]
groupBy : Int -> [a] -> [[a]]
groupBy n l = case l of
    [] -> []
    l -> (take n l) :: groupBy n (drop n l)

