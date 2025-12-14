module Storage.Queries where

import           Data.Int
import           Data.Map
import           Data.Text
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

selectFullBookById :: Int32 -> Pg (Maybe FullBook)
selectFullBookById bid = do
  mBook <-
    runSelectReturningOne $
      select $ do
        b <- all_ (_books libraryDb)
        guard_ (_bookId b ==. val_ bid)
        pure b

  case mBook of
    Nothing -> pure Nothing
    Just b -> do
      authors <-
        runSelectReturningList $
          select $ do
            ba <- all_ (_book_authors libraryDb)
            a  <- all_ (_authors libraryDb)
            guard_ (_baBookId ba ==. val_ (BookId bid))
            guard_ (_baAuthorId ba ==. pk a)
            pure a

      genres <-
        runSelectReturningList $
          select $ do
            bg <- all_ (_book_genres libraryDb)
            g  <- all_ (_genres libraryDb)
            guard_ (_bgBookId bg ==. val_ (BookId bid))
            guard_ (_bgGenreId bg ==. pk g)
            pure g

      pure $ Just (FullBook b authors genres)

selectAuthors :: Pg [Author]
selectAuthors =
  runSelectReturningList $
    select $ all_ (_authors libraryDb)

selectGenres :: Pg [Genre]
selectGenres =
  runSelectReturningList $
    select $ all_ (_genres libraryDb)

selectBookYears :: Pg [Int32]
selectBookYears =
  runSelectReturningList $
    select $ do
      b <- all_ (_books libraryDb)
      pure (_year b)

-- Пагинация + фильтры:
-- q         - поиск по названию (подстрока, регистр как есть, но обычно хватает)
-- authorId  - фильтр по автору (id)
-- genreId   - фильтр по жанру (id)
-- yearFrom/yearTo - диапазон по году (включительно)

selectBooksPagedFiltered
  :: Int -> Int
  -> Maybe Text
  -> Maybe Int32
  -> Maybe Int32
  -> Maybe Text   -- author_q
  -> Maybe Text   -- genre_q
  -> Maybe Int32
  -> Maybe Int32
  -> Pg [Book]
selectBooksPagedFiltered off lim mq mAuthor mGenre mAuthorQ mGenreQ mYFrom mYTo =
  runSelectReturningList $
    select $
      limit_ (fromIntegral lim) $
      offset_ (fromIntegral off) $ do

        b <- all_ (_books libraryDb)

        -- yearFrom/yearTo
        case mYFrom of
          Nothing -> pure ()
          Just y  -> guard_ (_year b >=. val_ y)

        case mYTo of
          Nothing -> pure ()
          Just y  -> guard_ (_year b <=. val_ y)

        case mq of
          Nothing -> pure ()
          Just q  ->
            let pat = "%" <> q <> "%"
            in guard_ (_title b `ilike_` val_ pat)

        -- фильтр по автору через EXISTS
        case mAuthor of
          Nothing -> pure ()
          Just aid ->
            guard_ $
              exists_ $ do
                ba <- all_ (_book_authors libraryDb)
                guard_ (_baBookId ba ==. pk b)
                guard_ (_baAuthorId ba ==. AuthorId (val_ aid))
                pure ba

        -- фильтр по жанру через EXISTS
        case mGenre of
          Nothing -> pure ()
          Just gid ->
            guard_ $
              exists_ $ do
                bg <- all_ (_book_genres libraryDb)
                guard_ (_bgBookId bg ==. pk b)
                guard_ (_bgGenreId bg ==. GenreId (val_ gid))
                pure bg

        case mAuthorQ of
          Nothing -> pure ()
          Just aq -> guard_ $
            exists_ $ do
              ba <- all_ (_book_authors libraryDb)
              a  <- all_ (_authors libraryDb)
              guard_ (_baBookId ba ==. pk b)
              guard_ (_baAuthorId ba ==. pk a)
              guard_ (_authorName a `ilike_` val_ ("%" <> aq <> "%"))
              pure ba

        -- genre_q (по имени жанра)
        case mGenreQ of
          Nothing -> pure ()
          Just gq -> guard_ $
            exists_ $ do
              bg <- all_ (_book_genres libraryDb)
              g  <- all_ (_genres libraryDb)
              guard_ (_bgBookId bg ==. pk b)
              guard_ (_bgGenreId bg ==. pk g)
              guard_ (_genreName g `ilike_` val_ ("%" <> gq <> "%"))
              pure bg

        pure b

selectTitleById :: Int32 -> Pg (Maybe Text)
selectTitleById bid = do
  runSelectReturningOne $
    select $ do
      b <- all_ (_books libraryDb)
      guard_ (_bookId b ==. val_ bid)
      pure (_title b)