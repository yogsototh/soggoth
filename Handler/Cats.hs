{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Handler.Cats where

import Import

getCatsR :: Handler Html
getCatsR = do
  defaultLayout $ do
    setTitle "Some Cats"
    $(widgetFile "cats")

dbdo dbAction objname msg = do
  result <- runDB dbAction
  case result of
      Right uid -> selectRep $ do
        provideRep $ defaultLayout [whamlet|Done|]
        provideRep $ return $ object [objname .= (object ["id" .= uid])]
      Left _ -> selectRep $ do
        provideRep $ defaultLayout [whamlet|#{msg}|]
        provideRep $ return $
          object ["error" .= ("This Cat Already Exists!" :: Text)]

postNewCatR :: Handler TypedContent
postNewCatR = do
  cat <- runInputPost $ Cat
            <$> ireq textField "name"
            <*> iopt intField "age"
  dbdo (insertBy cat) "cats" ("New Cat Created" :: String)

getCatR :: Text -> Handler Html
getCatR _ = error "getCatR not yet defined"

putCatR :: Text -> Handler Html
putCatR _ = error "putCatR not yet defined"

deleteCatR :: Text -> Handler Html
deleteCatR _ = error "deleteCatR not yet defined"
