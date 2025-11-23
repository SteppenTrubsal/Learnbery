{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TypeFamilies    #-}
module Storage.Types where

import           Control.Lens

import           Data.Text
import           Database.Beam
import           Database.Beam.Postgres

data AuthorT f = AuthorT
  { _authorId   :: C f Int
  , _authorName :: C f Text
  }
  deriving (Generic, Beamable)

type Author   = AuthorT Identity
type AuthorId = PrimaryKey AuthorT Identity

instance Table AuthorT where
  data PrimaryKey AuthorT f = AuthorId (C f Int)
    deriving (Generic, Beamable)
  primaryKey = AuthorId . _authorId

data GenreT f = GenreT
  { _genreId   :: C f Int
  , _genreName :: C f Text
  }
  deriving (Generic, Beamable)

type Genre   = GenreT Identity
type GenreId = PrimaryKey GenreT Identity

instance Table GenreT where
  data PrimaryKey GenreT f = GenreId (C f Int)
    deriving (Generic, Beamable)
  primaryKey = GenreId . _genreId

data BookT f = BookT
  { _bookId :: C f Int
  , _title  :: C f Text
  , _desc   :: C f Text
  , _year   :: C f Int
  , _pages  :: C f Int
  }
  deriving (Generic, Beamable)

type Book = BookT Identity
type BookId = PrimaryKey BookT Identity

instance Table BookT where
  data PrimaryKey BookT f = BookId (C f Int)
    deriving (Generic, Beamable)
  primaryKey = BookId . _bookId

data BookAuthorT f = BookAuthorT
  { _baBookId   :: PrimaryKey BookT f
  , _baAuthorId :: PrimaryKey AuthorT f
  }
  deriving (Generic, Beamable)

type BookAuthor   = BookAuthorT Identity
type BookAuthorId = PrimaryKey BookAuthorT Identity

instance Table BookAuthorT where
  data PrimaryKey BookAuthorT f = BookAuthorId (PrimaryKey BookT f) (PrimaryKey AuthorT f)
    deriving (Generic, Beamable)
  primaryKey (BookAuthorT b a) = BookAuthorId b a

data BookGenreT f = BookGenreT
  { _bgBookId  :: PrimaryKey BookT f
  , _bgGenreId :: PrimaryKey GenreT f
  }
  deriving (Generic, Beamable)

type BookGenre = BookGenreT Identity
type BookGenreId = PrimaryKey BookGenreT Identity

instance Table BookGenreT where
  data PrimaryKey BookGenreT f = BookGenreId (PrimaryKey BookT f) (PrimaryKey GenreT f)
    deriving (Generic, Beamable)
  primaryKey (BookGenreT b g) = BookGenreId b g

data LibraryDb f = LibraryDb
  { _authors     :: f (TableEntity AuthorT)
  , _books       :: f (TableEntity BookT)
  , _bookAuthors :: f (TableEntity BookAuthorT)
  , _bookGenres  :: f (TableEntity BookGenreT)
  , _genres      :: f (TableEntity GenreT)
  }
  deriving (Generic, Database Postgres)