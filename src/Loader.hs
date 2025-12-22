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
  actionэ <- execParser optsInfo

  case actionэ of
    Insert insOpts -> runInsert insOpts
    Delete title  -> runDelete title

runInsert :: InsertOptions -> IO ()
runInsert insOpts = do
  linesInput <- loadLines (insMode insOpts)

  when (null linesInput) $ do
    putStrLn "No lines - nothing to do"
    exitFailure

  if insDryRun insOpts
    then do
      putStrLn "Dry-run mode."
      forM_ (zip [1 :: Int ..] linesInput) $ \(i, l) ->
        case parseBookLine l of
          Left err ->
            putStrLn $
              "Line " <> show i <> ": parsing err: " <> T.unpack err
          Right bi -> do
            putStrLn $ "Line " <> show i <> ": OK"
            putStrLn $ "  title   = " <> T.unpack (biTitle bi)
            putStrLn $ "  desc    = " <> T.unpack (biDesc bi)
            putStrLn $ "  year    = " <> show (biYear bi)
            putStrLn $ "  pages   = " <> show (biPages bi)
            putStrLn $ "  authors = " <> T.unpack (T.intercalate ", " (biAuthors bi))
            putStrLn $ "  genres  = " <> T.unpack (T.intercalate ", " (biGenres bi))
      putStrLn "Dry-run complete."
    else do
      conf <- loadConf
      let postgreConn = defPostgreConf (conf ^. postgre)

      putStrLn "Connecting to DB ..."
      conn <- connect postgreConn

      putStrLn "Loading into DB..."
      forM_ (zip [1 :: Int ..] linesInput) $ \(i, l) ->
        case parseBookLine l of
          Left err ->
            putStrLn $
              "Line " <> show i <> ": parsing err: " <> T.unpack err
          Right bi -> do
            putStrLn $ "Line " <> show i <> ": Writing into DB..."
            runBeamPostgres conn (insertBookWithRelations bi)
            putStrLn "Done."
      putStrLn "Loading done."

runDelete :: T.Text -> IO ()
runDelete title = do
  conf <- loadConf
  let postgreConn = defPostgreConf (conf ^. postgre)

  putStrLn "Connecting to DB ..."
  conn <- connect postgreConn

  putStrLn $ "Deleting books with title: " <> T.unpack title
  runBeamPostgres conn (deleteBooksByTitle title)
  putStrLn "Delete done."