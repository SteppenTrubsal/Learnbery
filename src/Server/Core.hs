{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Server.Core where

import           Control.Monad.Reader

import           Data.Pool
import           Database.Beam.Postgres

import           Config

data AppEnv = AppEnv
  { config  :: Config
  , bd      :: Pool Connection
  }

newtype App a = App
  { unApp :: ReaderT AppEnv IO a }
  deriving newtype
    ( Applicative
    , Functor
    , Monad
    , MonadIO
    , MonadReader AppEnv
    )

runApp :: AppEnv -> App a -> IO a
runApp env action =
  runReaderT (unApp action) env