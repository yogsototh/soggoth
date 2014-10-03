module Handler.Cats where

import Import

getCatsR :: Handler Html
getCatsR = do
  defaultLayout $(widgetFile "cats")
