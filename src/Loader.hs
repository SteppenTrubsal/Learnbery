module Loader where

import           Control.Lens
import           Control.Monad

import qualified Data.Text as T
import           Database.Beam.Postgres

import           Options.Applicative

import           System.Exit

import           Config
import           Config.Postgre
import           Loader.ArgsParser
import           Loader.File
import           Loader.Interactive
import           Loader.Queries
import           Loader.Types

loader :: IO ()
loader = do
  opts <- execParser optsInfo

  linesInput <- loadLines (optMode opts)

  when (null linesInput) $ do
    putStrLn "No lines - nothing to do"
    exitFailure

  if optDryRun opts
    then do
      putStrLn "Dry-run mode."
      forM_ (zip [1 :: Int ..] linesInput) $ \(i, l) ->
        case parseBookLine l of
          Left err -> putStrLn $
            "Line " <> show i <> ": parsing err: " <> T.unpack err
          Right bi -> do
            putStrLn $ "Line " <> show i <> ": OK"
            putStrLn $ "  title   = " <> T.unpack (biTitle bi)
            putStrLn $ "  desc    = " <> T.unpack (biDesc bi)
            putStrLn $ "  year    = " <> show (biYear bi)
            putStrLn $ "  pages   = " <> show (biPages bi)
            putStrLn $ "  authors = " <> show (biAuthors bi)
            putStrLn $ "  genres  = " <> show (biGenres bi)
      putStrLn "Dry-run complete."
    else do
      conf <- loadConf
      let postgreConn = defPostgreConf (conf ^. postgre)

      putStrLn "Connecting to DB ..."
      conn <- connect postgreConn

      putStrLn "Loading into DB..."
      forM_ (zip [1 :: Int ..] linesInput) $ \(i, l) ->
        case parseBookLine l of
          Left err -> putStrLn $
            "Line " <> show i <> ": parsing err:" <> T.unpack err
          Right bi -> do
            putStrLn $ "Line " <> show i <> ": Writing into DB..."
            runBeamPostgres conn (insertBookWithRelations bi)
            putStrLn "Done."
      putStrLn "Loading done."