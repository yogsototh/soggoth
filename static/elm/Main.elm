module Main where

import Cat (..)
import Array
import Http
import Signal
import Json
import Dict
import Maybe
import Window

rotationSpeed = 1/5000
gonSize = 150
elapsedTime = foldp (+) 0 (fps 30)

main : Signal Element
main = display <~ Window.dimensions ~ elapsedTime ~ getJson "/cats"

display : (Int,Int) -> Float -> Http.Response String -> Element
display (w,h) t response = case response of
    Http.Success cats -> showCats (w,h) t <| jsonToCats (Json.fromString cats)
    Http.Waiting -> asText "WAITING..."
    Http.Failure _ _ -> asText response

showCats : (Int,Int) -> Float -> Maybe Cats -> Element
showCats (w,h) t cats = let containerSize = 200
                            nbOnRow = div w 200
                        in
  case cats of
    (Just (Cats r)) -> let drawOneLine oneLine = flow right <|
                                map (container containerSize containerSize middle . showOneCat t) oneLine
                       in container w h midTop <|
                            flow down <| map drawOneLine (listToGrid nbOnRow (.cats r))
    _ -> collage w h [rotate (t*rotationSpeed) (filled red (ngon 4 50))]


listToGrid : Int -> [a] -> [[a]]
listToGrid n l = case l of
    [] -> []
    l -> (take n l) :: listToGrid n (drop n l)

showOneCat : Float -> Cat -> Element
showOneCat t (Cat {age, name}) =
    let shape = case age of
                    Nothing -> oval (gonSize - 10) (gonSize - 20)
                    Just 0 -> oval (gonSize - 20) gonSize
                    Just 1 -> oval (gonSize - 20) gonSize
                    Just 2 -> rect (gonSize/2) gonSize
                    Just n -> ngon n (gonSize/2)
        ccolor = case age of
                    Nothing -> grey
                    Just 0 -> rgba 255 220 220 1.0
                    Just 1 -> red
                    Just 2 -> orange
                    Just 3 -> yellow
                    Just 4 -> green
                    Just 5 -> blue
                    Just n -> let level = (div 255 n) in
                                 rgba level level level 1.0
    in
        layers [ collage (round gonSize) (round gonSize) [rotate (t*rotationSpeed) <| move (0,0) (filled ccolor shape)]
               , container (round gonSize) (round gonSize) middle (centered (Text.color (rgba 0 0 0 0.6) (bold (toText name))))
               ]

-- HTTP Handling
jsonGetRequest : String -> Http.Request String
jsonGetRequest url = Http.request "GET" url "" [("Accept","application/json")]

getJson : String -> Signal (Http.Response String)
getJson url = Http.send <| Signal.constant <| jsonGetRequest <| url

