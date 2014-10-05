{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Handler.Cats where

import Import
import qualified Data.HashMap.Strict as HashMap

(+=) :: Value -> Value -> Value
(+=) (Object o1) (Object o2) = Object (HashMap.union o1 o2)
(+=) (Object o1) _ = Object o1
(+=) _ (Object o2) = Object o2
(+=) _ _ = Object HashMap.empty


getCatsR :: Handler Html
getCatsR = do
  defaultLayout $ do
    setTitle "Some Cats"
    $(widgetFile "cats")

create obj objname errmsg = do
  result <- runDB $ insertBy obj
  case result of
      Right uid -> selectRep $ do
        provideRep $ defaultLayout [whamlet|Done|]
        provideRep $ return
          $ object [objname .= (
              (object ["id" .= uid]) += (toJSON obj))]
      Left _ -> selectRep $ do
        provideRep $ defaultLayout [whamlet|#{errmsg}|]
        provideRep $ return $
          object ["error" .= errmsg]

postNewCatR :: Handler TypedContent
postNewCatR = do
  cat <- runInputPost $ Cat
            <$> ireq textField "name"
            <*> iopt intField "age"
  create cat "cats" ("Cat Already exists" :: String)


dbget dbAction objname msg = do
  result <- runDB dbAction
  case result of
      Just (Entity _ obj) -> selectRep $ do
        provideRep $ defaultLayout [whamlet|Done|]
        provideRep $ return $ object [objname .= (toJSON obj)]
      Nothing -> selectRep $ do
        provideRep $ defaultLayout [whamlet|#{msg}|]
        provideRep $ return $
          object ["error" .= msg ]

getCatR :: Text -> Handler TypedContent
getCatR catUniqueName = do
  dbget (getBy $ UniqueCat catUniqueName) "cats" ("Can't find the cat" :: String)

putCatR :: Text -> Handler Html
putCatR _ = error "putCatR not yet defined"

deleteCatR :: Text -> Handler Html
deleteCatR _ = error "deleteCatR not yet defined"
