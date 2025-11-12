module Server where

import           Network.Wai
import           Network.HTTP.Types
import           Lucid
import           Data.Text.Lazy.Encoding (encodeUtf8)

import           HTML.Main

app :: Application
app _ res =
  res $ responseLBS status200 [("Content-Type", "text/html; charset=utf-8")] (encodeUtf8 $ renderText bookPage)