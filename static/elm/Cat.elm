module Cat where

import Http
import Signal
import Json
import Dict
import Maybe

jsonGet : String -> Http.Request String
jsonGet url = Http.request "GET" url "" [("Accept","application/json")]

getCats : Http.Request String
getCats = jsonGet "/cats"

responses : Signal (Http.Response String)
responses = Http.send (Signal.constant getCats)

-- For Parsing
data Cats = Cats { cats : [Cat] }
data Cat  = Cat { name : String, age : Maybe Int }

catFromDict : Json.Value -> Maybe Cat
catFromDict val = case val of
    Json.Object dict ->
        case Dict.get "name" dict of
            Just (Json.String inst_name) -> case Dict.get "age" dict of
                Just (Json.Number inst_age) -> Just <| Cat { name = inst_name, age = Just (round inst_age) }
                Just (Json.Null) -> Just <| Cat { name = inst_name, age = Nothing }
                _ -> Nothing
            _ -> Nothing
    _ -> Nothing

fromJust : Maybe.Maybe a -> a
fromJust (Maybe.Just x) = x

objlistToCatList : [Json.Value] -> Maybe [Cat]
objlistToCatList dictList = let jlist = map catFromDict dictList in
    if all Maybe.isJust jlist
       then
        Just (map fromJust jlist)
       else
        Nothing


jsonToCats : Maybe Json.Value -> Maybe Cats
jsonToCats jcat = case jcat of
    Just (Json.Object dict) -> case Dict.get "cats" dict of
        Just (Json.Array objlist) -> case objlistToCatList objlist of
            Just catList -> Just (Cats { cats = catList })
            Nothing -> Nothing
        _ -> Nothing
    _ -> Nothing

-- End of Parsing


display : Http.Response String -> Element
display response = case response of
    Http.Success cats -> flow down [asText <| jsonToCats (Json.fromString cats)]
    Http.Waiting -> asText "WAITING"
    Http.Failure _ _ -> asText response

main : Signal Element
main = display <~ responses
