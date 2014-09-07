module Cat where

import Json
import Dict (get)
import Maybe (..)

data Cats = Cats { cats : [Cat] }
data Cat  = Cat { name : String, age : Maybe Int }

catFromDict : Json.Value -> Maybe Cat
catFromDict val = case val of
    Json.Object dict ->
        case get "name" dict of
            Just (Json.String inst_name) -> case get "age" dict of
                Just (Json.Number inst_age) -> Just <| Cat { name = inst_name, age = Just (round inst_age) }
                Just (Json.Null) -> Just <| Cat { name = inst_name, age = Nothing }
                _ -> Nothing
            _ -> Nothing
    _ -> Nothing

fromJust : Maybe a -> a
fromJust (Just x) = x

objlistToCatList : [Json.Value] -> Maybe [Cat]
objlistToCatList dictList = let jlist = map catFromDict dictList in
    if all isJust jlist
       then
        Just (map fromJust jlist)
       else
        Nothing


jsonToCats : Maybe Json.Value -> Maybe Cats
jsonToCats jcat = case jcat of
    Just (Json.Object dict) -> case get "cats" dict of
        Just (Json.Array objlist) -> case objlistToCatList objlist of
            Just catList -> Just (Cats { cats = catList })
            Nothing -> Nothing
        _ -> Nothing
    _ -> Nothing
