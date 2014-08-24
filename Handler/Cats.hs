module Handler.Cats where

import Import

answer :: ToJSON a => a -> Text -> Either l r -> Handler TypedContent
answer obj msg result = do
 case result of
    Right _ -> selectRep $ do
        provideRep $ defaultLayout [whamlet|Done|]
        provideRep $ return $ toJSON obj
    Left _ -> selectRep $ do
        provideRep $ defaultLayout [whamlet|#{msg}|]
        provideRep $ return $ object ["error" .= (msg :: Text)]

getCatsR :: Handler Html
getCatsR = do
  defaultLayout $ do
    setTitle "Some Cats"
    $(widgetFile "cats")

putCatsR :: Handler TypedContent
putCatsR = do
  -- Get the POST Parameters and Put them in a Cat Data structure
  cat <- runInputPost $ Cat
                <$> ireq textField "name"
                <*> iopt intField  "age"
  -- Insert the new cat in the DB
  result <- runDB $ insertBy cat
  answer cat "This Cat Already Exists!" result


askDB objuid = do
  maybeCat <- runDB $ getBy $ objuid
  case maybeCat of
    Nothing -> notFound
    Just (Entity _ obj) -> return obj

getCatR :: Text -> Handler TypedContent
getCatR catUniqueName = do
  cat <- askDB (UniqueCat catUniqueName)
  selectRep $ do
      provideRep $ defaultLayout [whamlet|<h2>I am #{catName cat}|]
      provideRep $ return $ toJSON cat

putCatR :: Text -> Handler Html
putCatR _ = error "getCatR not yet defined"

deleteCatR :: Text -> Handler Html
deleteCatR _ = error "getCatR not yet defined"
