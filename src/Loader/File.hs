{-# LANGUAGE ScopedTypeVariables #-}
module Loader.File where

import           Control.Exception

import qualified Data.Text      as T
import           Data.Text      (Text)
import qualified Data.Text.IO   as TI

import           Loader.Types

dataInputPrompt :: IO ()
dataInputPrompt = do
  putStrLn "String format:"
  putStrLn "  Title | Desc | Year | Pages | Author1, Author2 | Genre1, Genre2"
  putStrLn "Empty line to exit"
  putStrLn ""

loadLines :: Mode -> IO [Text]
loadLines Interactive = do
  dataInputPrompt
  let loop acc = do
        eofOrLine <- (Right <$> TI.getLine)
                      `catch` (\(_ :: SomeException) -> pure (Left ()))
        case eofOrLine of
          Left _ -> pure (reverse acc)
          Right line ->
            if T.null (T.strip line)
              then pure (reverse acc)
              else loop (line : acc)
  loop []

loadLines (File path) = do
  putStrLn $ "Reading file: " <> path
  content <- TI.readFile path
  let ls = filter (not . T.null . T.strip) (T.lines content)
  putStrLn $ "Lines found: " <> show (length ls)
  pure ls