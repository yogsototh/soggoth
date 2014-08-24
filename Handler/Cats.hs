module Handler.Cats where

import Import

getCatsR :: Handler Html
getCatsR = do
  defaultLayout $ do
    setTitle "Some Cats"
    $(widgetFile "cats")

putCatsR :: Handler Html
putCatsR = do
  -- Get the POST Parameters and Put them in a Cat Data structure
  cat <- runInputPost $ Cat
                <$> ireq textField "name"
                <*> iopt intField  "age"
  -- Insert the new cat in the DB
  runDB $ insert cat
  defaultLayout [whamlet|New Cat Created|]

getCatR :: Text -> Handler Html
getCatR _ = error "getCatR not yet defined"

putCatR :: Text -> Handler Html
putCatR _ = error "getCatR not yet defined"

deleteCatR :: Text -> Handler Html
deleteCatR _ = error "getCatR not yet defined"
