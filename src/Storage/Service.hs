{-# LANGUAGE ScopedTypeVariables #-}
module Storage.Service where

import           System.Directory
import           Control.Exception

safeListDir :: FilePath -> IO [FilePath]
safeListDir dir = listDirectory dir `catch` (\(_ :: IOException) -> pure [])

findFirstByPrefix :: String -> [FilePath] -> Maybe FilePath
findFirstByPrefix prefix files =
  case filter (\f -> take (length prefix) f == prefix) files of
    (x:_) -> Just x
    []    -> Nothing