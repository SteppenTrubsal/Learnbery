module Main where

import           Control.Lens

import           Database.Beam.Postgres
import           Database.Beam.Postgres.Migrate
import           Database.Beam.Migrate
import           Database.Beam.Migrate.Simple

import           Config
import           Config.Postgre
import           Storage.Schema

checkedLibraryDb :: CheckedDatabaseSettings Postgres LibraryDb
checkedLibraryDb = defaultMigratableDbSettings

main :: IO ()
main = do
  conf <- loadConf
  let postgreConn = defPostgreConf $ conf ^. postgre

  conn <- connect postgreConn

  putStrLn "Running autoMigrate..."
  runBeamPostgres conn $
    autoMigrate migrationBackend checkedLibraryDb

  putStrLn "Done. Schema should now be up-to-date."