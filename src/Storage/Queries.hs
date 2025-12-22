{-# LANGUAGE ScopedTypeVariables #-}
module Storage.Queries where

import           Data.Int
import           Data.Map
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Database.Beam
import           Database.Beam.Postgres

import           Storage.Types
import           Storage.Schema
import Data.Maybe (fromMaybe)

threshold :: Float
threshold = 0.10

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

        case mAuthor of
          Nothing -> pure ()
          Just aid ->
            guard_ $
              exists_ $ do
                ba <- all_ (_book_authors libraryDb)
                guard_ (_baBookId ba ==. pk b)
                guard_ (_baAuthorId ba ==. AuthorId (val_ aid))
                pure ba

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

trgmMatch_ :: QExpr Postgres s Text -> QExpr Postgres s Text -> QExpr Postgres s Bool
trgmMatch_ = customExpr_ (\a b -> "(" <> a <> " % " <> b <> ")")

trgmSimilarity_ :: QExpr Postgres s Text -> QExpr Postgres s Text -> QExpr Postgres s Float
trgmSimilarity_ = customExpr_ (\a b -> "similarity(" <> a <> ", " <> b <> ")")

trgmMatchCI_ :: QExpr Postgres s Text -> QExpr Postgres s Text -> QExpr Postgres s Bool
trgmMatchCI_ a b = trgmMatch_ (lower_ a) (lower_ b)

trgmSimilarityCI_ :: QExpr Postgres s Text -> QExpr Postgres s Text -> QExpr Postgres s Float
trgmSimilarityCI_ a b = trgmSimilarity_ (lower_ a) (lower_ b)

normalizeQ :: Maybe Text -> Maybe Text
normalizeQ = (>>= \t -> let t' = T.strip t in if T.null t' then Nothing else Just t')

whenJustQ_ :: Maybe a -> (a -> Q Postgres LibraryDb s ()) -> Q Postgres LibraryDb s ()
whenJustQ_ ma act = maybe (pure ()) act ma

guardTrgmOrIlikeCI_
  :: QExpr Postgres s Text
  -> Text
  -> Q Postgres LibraryDb s ()
guardTrgmOrIlikeCI_ field q
  | T.length q < 3 =
      let pat = "%" <> q <> "%"
      in guard_ (field `ilike_` val_ pat)
  | otherwise =
      guard_ (trgmSimilarityCI_ field (val_ q) >=. val_ threshold)

type BookRow s = BookT (QExpr Postgres s)

applyYearRange_
  :: Maybe Int32 -> Maybe Int32
  -> BookRow s
  -> Q Postgres LibraryDb s ()
applyYearRange_ mYFrom mYTo b = do
  whenJustQ_ mYFrom $ \y -> guard_ (_year b >=. val_ y)
  whenJustQ_ mYTo   $ \y -> guard_ (_year b <=. val_ y)

applyTitleQ_
  :: Maybe Text
  -> BookRow s
  -> Q Postgres LibraryDb s ()
applyTitleQ_ mq b =
  whenJustQ_ (normalizeQ mq) $ \q ->
    guardTrgmOrIlikeCI_ (_title b) q

applyAuthorId_
  :: Maybe Int32
  -> BookRow s
  -> Q Postgres LibraryDb s ()
applyAuthorId_ mAuthor b =
  whenJustQ_ mAuthor $ \aid ->
    guard_ $
      exists_ $ do
        ba <- all_ (_book_authors libraryDb)
        guard_ (_baBookId ba   ==. pk b)
        guard_ (_baAuthorId ba ==. AuthorId (val_ aid))
        pure ba

applyGenreId_
  :: Maybe Int32
  -> BookRow s
  -> Q Postgres LibraryDb s ()
applyGenreId_ mGenre b =
  whenJustQ_ mGenre $ \gid ->
    guard_ $
      exists_ $ do
        bg <- all_ (_book_genres libraryDb)
        guard_ (_bgBookId bg  ==. pk b)
        guard_ (_bgGenreId bg ==. GenreId (val_ gid))
        pure bg

applyAuthorQ_
  :: Maybe Text
  -> BookRow s
  -> Q Postgres LibraryDb s ()
applyAuthorQ_ mAuthorQ b =
  whenJustQ_ (normalizeQ mAuthorQ) $ \aq ->
    guard_ $
      exists_ $ do
        ba <- all_ (_book_authors libraryDb)
        a  <- all_ (_authors libraryDb)
        guard_ (_baBookId ba   ==. pk b)
        guard_ (_baAuthorId ba ==. pk a)
        guardTrgmOrIlikeCI_ (_authorName a) aq
        pure ba

applyGenreQ_
  :: Maybe Text
  -> BookRow s
  -> Q Postgres LibraryDb s ()
applyGenreQ_ mGenreQ b =
  whenJustQ_ (normalizeQ mGenreQ) $ \gq ->
    guard_ $
      exists_ $ do
        bg <- all_ (_book_genres libraryDb)
        g  <- all_ (_genres libraryDb)
        guard_ (_bgBookId bg  ==. pk b)
        guard_ (_bgGenreId bg ==. pk g)
        guardTrgmOrIlikeCI_ (_genreName g) gq
        pure bg

titleScore_
  :: Text
  -> BookT (QExpr Postgres s)
  -> QExpr Postgres s Float
titleScore_ q b = trgmSimilarityCI_ (_title b) (val_ q)

selectBooksPagedFilteredT
  :: Int -> Int
  -> Maybe Text
  -> Maybe Int32
  -> Maybe Int32
  -> Maybe Text
  -> Maybe Text
  -> Maybe Int32
  -> Maybe Int32
  -> Pg [Book]
selectBooksPagedFilteredT off lim mq mAuthor mGenre mAuthorQ mGenreQ mYFrom mYTo =
  runSelectReturningList $
    select $
      limit_  (fromIntegral lim) $
      offset_ (fromIntegral off) $
      orderBy_ (\b ->
        let score =
              if useRelevance
                then titleScore_ qNorm b
                else val_ (0 :: Float)
        in (desc_ score, asc_ (_bookId b))
      ) $ do
        b <- all_ (_books libraryDb)

        maybe (pure ()) (\y -> guard_ (_year b >=. val_ y)) mYFrom
        maybe (pure ()) (\y -> guard_ (_year b <=. val_ y)) mYTo

        applyTitleQ_ mq b

        applyAuthorId_ mAuthor b

        applyGenreId_ mGenre b

        applyAuthorQ_ mAuthorQ b
        applyGenreQ_  mGenreQ  b

        pure b
  where
    qNorm :: Text
    qNorm = fromMaybe "" (normalizeQ mq)

    useRelevance :: Bool
    useRelevance = T.length qNorm >= 3

selectBooksByTitlePaged
  :: Int -> Int
  -> Maybe Text
  -> Pg [Book]
selectBooksByTitlePaged off lim mq =
  runSelectReturningList $
    select $
      limit_  (fromIntegral lim) $
      offset_ (fromIntegral off) $
      orderBy_ (\b ->
        let score =
              case normalizeQ mq of
                Just q | T.length q >= 3 -> trgmSimilarityCI_ (_title b) (val_ q)
                _                        -> val_ (0 :: Float)
        in (desc_ score, asc_ (_bookId b))
      ) $ do
        b <- all_ (_books libraryDb)
        case normalizeQ mq of
          Nothing -> guard_ (val_ False)
          Just q  -> guardTrgmOrIlikeCI_ (_title b) q
        pure b