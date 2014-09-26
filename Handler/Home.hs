{-# LANGUAGE OverloadedStrings #-}
module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
  defaultLayout $ do
      setTitle "Soggoth Yesod Example"
      $(widgetFile "homepage")

