module Server where

import           Control.Lens

import           Data.Pool
import           Database.Beam.Postgres

import           Network.Wai
import qualified Network.Wai.Handler.Warp as W
import           Network.Wai.Middleware.Static

import           Server.Core
import           Server.Router
import           Config                   ( Config, common, loadConf, postgre, pool )
import           Config.Common
import           Config.Pool
import qualified Config.Postgre           as CP

launch :: IO ()
launch = do
  conf <- loadConf
  pool' <- mkPool conf 
  mw <- staticMw conf
  putStrLn $ "Starting server on " ++ show (conf ^. common . port)
  let
    appEnv = AppEnv conf pool'
    app    = mkApplication appEnv
  W.run (conf ^. common . port) $ mw app
  where
    mkPool conf = newPool $ 
      defaultPoolConfig
        (connect $ CP.defPostgreConf (conf ^. postgre))
        close
        (conf ^. pool . keepAlive)
        (conf ^. pool . resPerStripe)

staticMw :: Config -> IO Middleware 
staticMw conf = do
  cache <- initCaching PublicStaticCaching
  let
    public    = conf ^. common . siteDir
    resources = conf ^. common . resDir

    opts = defaultOptions {cacheContainer = cache}

    staticDir dir = staticPolicyWithOptions opts (addBase dir)
  
  pure $ staticDir resources . staticDir public

mkApplication :: AppEnv -> Application
mkApplication env req res = do
  runApp env (router req res)