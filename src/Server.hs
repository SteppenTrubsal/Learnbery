module Server where

import           Control.Lens

import           Data.Pool
import           Database.Beam.Postgres

import qualified Network.Wai.Handler.Warp as W
import           Network.Wai

import           Server.Core
import           Server.Router
import           Config                   ( common, loadConf, postgre, pool )
import           Config.Common
import           Config.Pool
import qualified Config.Postgre           as CP

launch :: IO ()
launch = do
  conf <- loadConf
  pool' <- mkPool conf 
  putStrLn $ "Starting server on " ++ show (conf ^. common . port)
  let
    appEnv = AppEnv conf pool'
    app    = mkApplication appEnv
  W.run (conf ^. common . port) app
  where
    mkPool conf = newPool $ 
      defaultPoolConfig
        (connect $ CP.defPostgreConf (conf ^. postgre))
        close
        (conf ^. pool . keepAlive)
        (conf ^. pool . resPerStripe)

mkApplication :: AppEnv -> Application
mkApplication env req res = do
  runApp env (router req res)