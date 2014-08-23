module Handler.Cats where

import Import

getCatsR :: Handler Html
getCatsR = do
  defaultLayout $ do
    setTitle "Some Cats"
    $(widgetFile "cats")

putCatsR :: Handler Html
putCatsR = error "putCatsR not yet defined"

getCatR :: Text -> Handler Html
getCatR _ = error "getCatR not yet defined"

putCatR :: Text -> Handler Html
putCatR _ = error "getCatR not yet defined"

deleteCatR :: Text -> Handler Html
deleteCatR _ = error "getCatR not yet defined"
