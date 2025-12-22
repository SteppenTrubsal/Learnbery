module Loader.Types where

import           Data.Int
import           Data.Text

data Mode
  = Interactive
  | File FilePath
  deriving (Show)

data InsertOptions = InsertOptions
  { insMode   :: Mode
  , insDryRun :: Bool
  } deriving (Show)

data Action
  = Insert InsertOptions
  | Delete Text
  deriving (Show)

data BookInput = BookInput
  { biTitle   :: Text
  , biDesc    :: Text
  , biYear    :: Int32
  , biPages   :: Int32
  , biAuthors :: [Text]
  , biGenres  :: [Text]
  }
  deriving (Show)