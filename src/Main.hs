module Main where

import           Control.Lens
import           Network.Wai.Handler.Warp as W

import           Config
import           Server

main :: IO ()
main = do
  conf <- loadConf
  putStrLn $ "Starting server on " ++ show (conf ^. port)
  W.run (conf ^. port) app