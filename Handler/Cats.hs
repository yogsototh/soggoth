module Handler.Cats where

import Import

getCatsR :: Handler Html
getCatsR = do
  defaultLayout $ do
    setTitle "Some Cats"
    $(widgetFile "cats")

postNewCatR :: Handler Html
postNewCatR = do
  cat <- runInputPost $ Cat
            <$> ireq textField "name"
            <*> iopt intField "age"
  _ <- runDB $ insert cat
  defaultLayout [whamlet|New Cat Created|]

getCatR :: Text -> Handler Html
getCatR _ = error "getCatR not yet defined"

putCatR :: Text -> Handler Html
putCatR _ = error "putCatR not yet defined"

deleteCatR :: Text -> Handler Html
deleteCatR _ = error "deleteCatR not yet defined"
