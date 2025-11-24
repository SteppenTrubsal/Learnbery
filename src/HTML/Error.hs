module HTML.Error where

import           Lucid

errorPage :: String -> Html ()
errorPage s = doctypehtml_ $ do
  head_ $ do
    title_ ""
    link_ [rel_ "stylesheet", href_ "css/error.css"]
  
  body_ $ do
    div_ [class_ "error-card", role_ "alert"] $ do
      p_ [class_ "error-text"] $ do
        toHtml $ show s