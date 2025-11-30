module Loader.Types where

import           Data.Int
import           Data.Text

data Mode
  = Interactive
  | File FilePath
  deriving (Show)


data Options = Options
  { optMode   :: Mode
  , optDryRun :: Bool
  } deriving (Show)

data BookInput = BookInput
  { biTitle   :: Text
  , biDesc    :: Text
  , biYear    :: Int32
  , biPages   :: Int32
  , biAuthors :: [Text]
  , biGenres  :: [Text]
  }
  deriving (Show)