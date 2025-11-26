module Server.Router where

import           Control.Monad.IO.Class

import           Network.Wai
import           Network.Wai.Application.Static
import           Network.HTTP.Types

import           HTML.Error
import           HTML.Main
import           Server.Core
import           Server.Utils
import           Storage.Queries

staticSettings :: StaticSettings
staticSettings = defaultFileServerSettings "public"

router :: Request -> (Response -> IO ResponseReceived) -> App ResponseReceived
router req res
  | isStatic (pathInfo req) = liftIO $ staticApp staticSettings req res
  | otherwise =
    case requestMethod req of
      "GET"  -> getRouter req res
      _      -> liftIO $ res $ pageResponse status400 (errorPage "Undefined http method - what did you do to get this)) ?")

getRouter :: Request -> (Response -> IO ResponseReceived) -> App ResponseReceived
getRouter req res =
  case rawPathInfo req of
    "/" ->
      liftIO $ res $ pageResponse status200 bookPage
    "/test" -> do
      fb <- runQuery selectFullBooks
      liftIO $ res $ pageResponse status200 $ dbTest fb
    _   ->
      liftIO $ res $ pageResponse status404 (errorPage "Page not found - nothing to see there")