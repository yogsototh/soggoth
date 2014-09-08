module Main where

import Cat (..)
import Array
import Http
import Signal
import Json
import Dict
import Maybe
import Window
import Grid (reactiveGrid)
import HSV (hsv)

--------------------------------------------------------------------------------
-- Constants
rotationSpeed = 1/5000
gonSize       = 150
elapsedTime   = foldp (+) 0 (fps 30)

--------------------------------------------------------------------------------
main : Signal Element
main = display <~ Window.dimensions
                ~ elapsedTime
                ~ getJson "/cats"

--------------------------------------------------------------------------------
display : (Int,Int) -- ^ Window dimension
          -> Float  -- ^ time since start
          -> Http.Response String --> ^ the Response of HTTP query
          -> Element
display (w,h) t response = case response of
    Http.Success cats -> showCats (w,h) t <| jsonToCats (Json.fromString cats)
    Http.Waiting      -> container w h middle <| plainText "WAITING..."
    Http.Failure _ _  -> asText response -- Something went wrong


--------------------------------------------------------------------------------
showCats : (Int,Int) -> Float -> Maybe Cats -> Element
showCats (w,h) t cats =
    case cats of
        (Just (Cats r)) -> reactiveGrid (200,200) (map (showOneCat t) (.cats r)) w
        _ -> collage w h [rotate (t*rotationSpeed) (filled red (ngon 4 50))]

--------------------------------------------------------------------------------
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
                    Just n -> let hue = toFloat (n * 36) in
                                 hsv hue 0.5 180.0
    in
        layers [ collage (round gonSize) (round gonSize) [rotate (t*rotationSpeed) <| move (0,0) (filled ccolor shape)]
               , container (round gonSize) (round gonSize) middle (centered (Text.color (rgba 0 0 0 0.6) (bold (toText name))))
               ]

-- HTTP Handling
jsonGetRequest : String -> Http.Request String
jsonGetRequest url = Http.request "GET" url "" [("Accept","application/json")]

getJson : String -> Signal (Http.Response String)
getJson url = Http.send <| Signal.constant <| jsonGetRequest <| url

