module Storage.Queries where

import           Data.Int
import           Data.Map
import           Database.Beam
import           Database.Beam.Postgres

import           Storage.Types
import           Storage.Schema

selectBooks :: Pg [Book]
selectBooks =
  runSelectReturningList $
    select $
      all_ (_books libraryDb)

selectBookAuthors :: Pg [(BookId, Author)]
selectBookAuthors =
  runSelectReturningList $
    select $ do
      ba <- all_ (_book_authors libraryDb)
      a  <- all_ (_authors     libraryDb)

      guard_ (_baAuthorId ba ==. pk a)

      pure (_baBookId ba, a)

selectBookGenres :: Pg [(BookId, Genre)]
selectBookGenres =
  runSelectReturningList $
    select $ do
      bg <- all_ (_book_genres libraryDb)
      g  <- all_ (_genres     libraryDb)

      guard_ (_bgGenreId bg ==. pk g)

      pure (_bgBookId bg, g)

selectFullBooks :: Pg [FullBook]
selectFullBooks = do
  books       <- selectBooks
  bookAuthors <- selectBookAuthors
  bookGenres  <- selectBookGenres

  let 
    authMap =
      fromListWith (<>)
        [(bid, [a]) | (BookId bid, a) <- bookAuthors]

    genrMap =
      fromListWith (<>)
        [(bid, [g]) | (BookId bid, g) <- bookGenres]

    fullBooks =
      [ FullBook
        b
        (findWithDefault [] bid authMap)
        (findWithDefault [] bid genrMap)
      | b <- books
      , let BookId bid = pk b
      ]

  pure fullBooks

selectBooksPaged :: Int -> Int -> Pg [Book]
selectBooksPaged off lim =
  runSelectReturningList $ 
    select $ 
      limit_ (fromIntegral lim) $ 
      offset_ (fromIntegral off) $ 
      all_ (_books libraryDb)

selectBookById :: Int32 -> Pg (Maybe Book)
selectBookById bid =
  runSelectReturningOne $
    select $ do
      b <- all_ (_books libraryDb)
      guard_ (_bookId b ==. val_ bid)
      pure b

selectAuthorsByBookId :: Int32 -> Pg [Author]
selectAuthorsByBookId bid =
  runSelectReturningList $
    select $ do
      ba <- all_ (_book_authors libraryDb)
      a  <- all_ (_authors libraryDb)
      guard_ (_baAuthorId ba ==. pk a)
      guard_ (_baBookId ba ==. val_ (BookId bid))
      pure a

selectGenresByBookId :: Int32 -> Pg [Genre]
selectGenresByBookId bid =
  runSelectReturningList $
    select $ do
      bg <- all_ (_book_genres libraryDb)
      g  <- all_ (_genres      libraryDb)
      guard_ (_bgGenreId bg ==. pk g)
      guard_ (_bgBookId bg ==. val_ (BookId bid))
      pure g
