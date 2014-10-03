module Handler.Cats where

import Import

getCatsR :: Handler Html
getCatsR = do
  defaultLayout $ do
    setTitle "Some Cats"
    $(widgetFile "cats")

postNewCatR :: Handler TypedContent
postNewCatR = do
  cat <- runInputPost $ Cat
            <$> ireq textField "name"
            <*> iopt intField "age"
  result <- runDB $ insertBy cat
  case result of
    Right uid -> selectRep $ do
      provideRep $ defaultLayout [whamlet|New Cat Created|]
      provideRep $ return $ toJSON cat
    Left (Entity uid _) -> selectRep $ do
      provideRep $ defaultLayout [whamlet|This Cat Already Exist!s|]
      provideRep $ return $
        object ["error" .= ("This Cat Already Exists!" :: Text)]

getCatR :: Text -> Handler Html
getCatR _ = error "getCatR not yet defined"

putCatR :: Text -> Handler Html
putCatR _ = error "putCatR not yet defined"

deleteCatR :: Text -> Handler Html
deleteCatR _ = error "deleteCatR not yet defined"
