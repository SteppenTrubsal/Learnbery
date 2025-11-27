module Storage.Types where

import           Control.Lens

import           Storage.Schema

data FullBook = FullBook
  { _book :: Book
  , _auth :: [Author]
  , _genr :: [Genre]
  } deriving (Show)
makeLenses ''FullBook