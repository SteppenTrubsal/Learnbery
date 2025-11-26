module Storage.RawTypes where

import           Control.Lens

import           Storage.Types

data FullBook = FullBook
  { _book :: Book
  , _auth :: [Author]
  , _genr :: [Genre]
  } deriving (Show)
makeLenses ''FullBook