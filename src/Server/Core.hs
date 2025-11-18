{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Server.Core where

import           Control.Monad.Reader

import           Config

data AppEnv = AppEnv
  { config   :: Config
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