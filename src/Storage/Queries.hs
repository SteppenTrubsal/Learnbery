module Storage.Queries where

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