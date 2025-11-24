module Server where

import           Control.Lens

import qualified Network.Wai.Handler.Warp as W
import           Network.Wai

import           Server.Core
import           Server.Router
import           Config
import           Config.Common

-- app :: Application
-- app _ res =
--   res $ responseLBS status200 [("Content-Type", "text/html; charset=utf-8")] (encodeUtf8 $ renderText bookPage)

launch :: IO ()
launch = do
  conf <- loadConf
  putStrLn $ "Starting server on " ++ show (conf ^. common . port)
  let
    appEnv = AppEnv conf
    app    = mkApplication appEnv
  W.run (conf ^. common . port) app

mkApplication :: AppEnv -> Application
mkApplication env req res = do
  runApp env (router req res)