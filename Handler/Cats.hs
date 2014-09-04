module Handler.Cats where

import Import
import Control.Monad (when)

answer :: ToJSON a => a -> Text -> Either l r -> Handler TypedContent
answer obj msg result = do
 case result of
    Right _ -> selectRep $ do
        provideRep $ defaultLayout [whamlet|Done|]
        provideRep $ return $ toJSON obj
    Left _ -> selectRep $ do
        provideRep $ defaultLayout [whamlet|#{msg}|]
        provideRep $ return $ object ["error" .= (msg :: Text)]

getCatsR :: Handler TypedContent
getCatsR = do
  cats <- runDB $ selectList [][LimitTo 100]
  selectRep $ do
    provideRep $ defaultLayout $ do
      setTitle "Some Cats"
      $(widgetFile "cats")
    provideRep $ do
      let catList = map (\ (Entity _ cat) -> cat) cats
      return $ object $ ["cats" .=  catList]

putCatsR :: Handler TypedContent
putCatsR = do
  -- Get the POST Parameters and Put them in a Cat Data structure
  cat <- runInputPost $ Cat
                <$> ireq textField "name"
                <*> iopt intField  "age"
  -- Insert the new cat in the DB
  result <- runDB $ insertBy cat
  answer cat "This Cat Already Exists!" result


askDB :: (YesodPersist site,
          PersistUnique (YesodPersistBackend site (HandlerT site IO)),
          PersistEntity b,
          PersistMonadBackend (YesodPersistBackend site (HandlerT site IO))
          ~ PersistEntityBackend b) =>
         Unique b -> HandlerT site IO b
askDB objuid = do
  maybeCat <- runDB $ getBy $ objuid
  case maybeCat of
    Nothing -> notFound
    Just (Entity _ obj) -> return obj

getCatR :: Text -> Handler TypedContent
getCatR catUniqueName = do
  cat <- askDB (UniqueCat catUniqueName)
  selectRep $ do
      provideRep $ defaultLayout $(widgetFile "cat")
      provideRep $ return $ toJSON cat

putCatR :: Text -> Handler TypedContent
putCatR catUniqueName = do
  -- get the parameters
  cat <- runInputPost $ Cat
                <$> ireq textField "name"
                <*> iopt intField  "age"
  -- ask the DB if such a cat already exists
  mCatEntity <- runDB $ getBy (UniqueCat catUniqueName)
  case mCatEntity of
    -- if not then get out of here
    Nothing -> notFound
    -- if such a cat already exists
    Just (Entity catId _) -> do
        -- check the parameter name match the url one
        when (catName cat /= catUniqueName) notFound
        -- update its ages
        _ <- runDB $ update catId [CatAge =. catAge cat]
        -- Answer nicely the new cat state
        selectRep $ do
            provideRep $ defaultLayout $(widgetFile "cat")
            provideRep $ return $ toJSON cat

deleteCatR :: Text -> Handler TypedContent
deleteCatR catUniqueName = do
    _ <- runDB $ do
        mCatEntity <- getBy (UniqueCat catUniqueName)
        case mCatEntity of
            Nothing -> notFound
            Just (Entity catId _) -> delete catId
    selectRep $ do
        provideRep $ defaultLayout [whamlet|#{catUniqueName} has been deleted.|]
        provideRep $ return $
            object ["msg" .= (catUniqueName <> " has been deleted")]

