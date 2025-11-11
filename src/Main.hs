module Main where

import           Control.Lens

import           Config

main :: IO ()
main = do
  conf <- loadConf
  putStrLn $ "Starting server on " ++ show (conf ^. port)