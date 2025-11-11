module Server where

import           Network.Wai
import           Network.HTTP.Types

app :: Application
app _ res =
  res $ responseLBS status200 [("Content-Type", "text/plain")] "pong"