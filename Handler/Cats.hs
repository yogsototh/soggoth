module Handler.Cats where

import Import

getCatsR :: Handler Html
getCatsR = do
  defaultLayout $ [whamlet|
    <div .hero-unit>
      <h1>Some Cats
    <div .container>
      <div .row>
        <div .span4>
          <h2>Persian
          <p>The Persian cat
        <div .span4>
          <h2>Savannah
          <p>The Savannah cat
        <div .span4>
          <h2>Siamese
          <p>The Siamese cat
        |]
