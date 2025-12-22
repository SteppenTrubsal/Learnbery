module Loader.Queries where

import           Control.Lens

import           Data.Text              (Text)
import           Data.Maybe
import           Control.Monad
import           Data.Foldable (traverse_)
import           Database.Beam
import           Database.Beam.Postgres
import           Database.Beam.Schema.Tables

import           Loader.Types
import           Storage.Schema

getOrCreateAuthor :: Text -> Pg AuthorId
getOrCreateAuthor name = do
  mAuthor <- runSelectReturningOne $ select $
                filter_ (\a -> _authorName a ==. val_ name) $
                all_ (_authors libraryDb)
  case mAuthor of
    Just a  -> pure (AuthorId (_authorId a))
    Nothing -> do
      runInsert $
        insert (_authors libraryDb) $
          insertExpressions
            [ AuthorT
                { _authorId   = default_
                , _authorName = val_ name
                }
            ]

      mAuthor' <- runSelectReturningOne $ select $
                    filter_ (\a -> _authorName a ==. val_ name) $
                    all_ (_authors libraryDb)

      case mAuthor' of
        Just a' -> pure (AuthorId (_authorId a'))
        Nothing -> fail "getOrCreateAuthor: insert succeeded but row not found"

getOrCreateGenre :: Text -> Pg GenreId
getOrCreateGenre name = do
  mGenre <- runSelectReturningOne $ select $
               filter_ (\g -> _genreName g ==. val_ name) $
               all_ (_genres libraryDb)
  
  case mGenre of
    Just g  -> pure (GenreId (_genreId g))
    Nothing -> do
      runInsert $
        insert (_genres libraryDb) $
          insertExpressions
            [ GenreT
                { _genreId   = default_
                , _genreName = val_ name
                }
            ]
      
      mGenre' <- runSelectReturningOne $ select $
        filter_ (\g -> _genreName g ==. val_ name) $
                   all_ (_genres libraryDb)

      case mGenre' of
        Just g' -> pure (GenreId (_genreId g'))
        Nothing -> fail "getOrCreateGenre: insert succeeded but row not found"

insertBookWithRelations :: BookInput -> Pg ()
insertBookWithRelations bi = do
  authorIds <- mapM getOrCreateAuthor (biAuthors bi)
  genreIds  <- mapM getOrCreateGenre  (biGenres bi)

  runInsert $
    insert (_books libraryDb) $
      insertExpressions
        [ BookT
            { _bookId = default_
            , _title  = val_ (biTitle bi)
            , _desc   = val_ (biDesc bi)
            , _year   = val_ (biYear bi)
            , _pages  = val_ (biPages bi)
            }
        ]

  mBook <- runSelectReturningOne $ select $
             limit_ 1 $
             orderBy_ (desc_ . _bookId) $
             all_ (_books libraryDb)

  book <- maybe (fail "insertBookWithRelations: can't find last inserted book")
                pure
                mBook

  let bookIdPk = BookId (_bookId book)

  runInsert $
    insert (_book_authors libraryDb) $
      insertValues
        [ BookAuthorT bookIdPk authorId | authorId <- authorIds ]

  runInsert $
    insert (_book_genres libraryDb) $
      insertValues
        [ BookGenreT bookIdPk genreId | genreId <- genreIds ]

  pure ()

testInsertBA :: Pg ()
testInsertBA = do
  let bId = BookId 1   -- или просто BookId 1, если Identity
      aId = AuthorId 1 -- аналогично
  runInsert $
    insert (_book_authors libraryDb) $
      insertValues [ BookAuthorT bId aId ]

printTableNames :: Pg ()
printTableNames = do
  liftIO $ do
    putStrLn $ "authors:     " ++ show (_authors     libraryDb ^. dbEntityDescriptor . dbEntityName)
    putStrLn $ "books:       " ++ show (_books       libraryDb ^. dbEntityDescriptor . dbEntityName)
    putStrLn $ "bookAuthors: " ++ show (_book_authors libraryDb ^. dbEntityDescriptor . dbEntityName)
    putStrLn $ "bookGenres:  " ++ show (_book_genres  libraryDb ^. dbEntityDescriptor . dbEntityName)
    putStrLn $ "genres:      " ++ show (_genres      libraryDb ^. dbEntityDescriptor . dbEntityName)

findBookByTitleMin :: Text -> Pg (Maybe Book)
findBookByTitleMin t =
  runSelectReturningOne $
    select $
      limit_ 1 $
      orderBy_ (asc_ . _bookId) $
      filter_ (\b -> _title b ==. val_ t) $
      all_ (_books libraryDb)

findTouchedAuthors :: BookId -> Pg [AuthorId]
findTouchedAuthors bookId =
  runSelectReturningList $
    select $
      do
        ba <- all_ (_book_authors libraryDb)
        guard_ (_baBookId ba ==. val_ bookId)
        pure (_baAuthorId ba)

findTouchedGenres :: BookId -> Pg [GenreId]
findTouchedGenres bookId =
  runSelectReturningList $
    select $
      do
        bg <- all_ (_book_genres libraryDb)
        guard_ (_bgBookId bg ==. val_ bookId)
        pure (_bgGenreId bg)

existsAuthorUsage :: AuthorId -> Pg Bool
existsAuthorUsage aid = do
  mb <- runSelectReturningOne $
    select $
      pure $
        exists_ $
          do
            ba <- all_ (_book_authors libraryDb)
            guard_ (_baAuthorId ba ==. val_ aid)
            pure (_baAuthorId ba)
  pure (fromMaybe False mb)

existsGenreUsage :: GenreId -> Pg Bool
existsGenreUsage gid = do
  mb <- runSelectReturningOne $
    select $
      pure $
        exists_ $
          do
            bg <- all_ (_book_genres libraryDb)
            guard_ (_bgGenreId bg ==. val_ gid)
            pure (_bgGenreId bg)
  pure (fromMaybe False mb)

deleteBookAuthors :: BookId -> Pg ()
deleteBookAuthors bookId =
  runDelete $
    delete (_book_authors libraryDb)
      (\ba -> _baBookId ba ==. val_ bookId)

deleteBookGenres :: BookId -> Pg ()
deleteBookGenres bookId =
  runDelete $
    delete (_book_genres libraryDb)
      (\bg -> _bgBookId bg ==. val_ bookId)

deleteBook :: BookId -> Pg ()
deleteBook bookId =
  runDelete $
    delete (_books libraryDb)
      (\b -> primaryKey b ==. val_ bookId)

deleteAuthorIfOrphan :: AuthorId -> Pg ()
deleteAuthorIfOrphan aid = do
  used <- existsAuthorUsage aid
  unless used $
    runDelete $
      delete (_authors libraryDb)
        (\a -> primaryKey a ==. val_ aid)

deleteGenreIfOrphan :: GenreId -> Pg ()
deleteGenreIfOrphan gid = do
  used <- existsGenreUsage gid
  unless used $
    runDelete $
      delete (_genres libraryDb)
        (\g -> primaryKey g ==. val_ gid)

deleteBooksByTitle :: Text -> Pg ()
deleteBooksByTitle title = do
  mbBook <- findBookByTitleMin title
  forM_ mbBook $ \book -> do
    let bookId = primaryKey book

    authors <- findTouchedAuthors bookId
    genres  <- findTouchedGenres bookId

    deleteBookAuthors bookId
    deleteBookGenres  bookId
    deleteBook        bookId

    traverse_ deleteAuthorIfOrphan authors
    traverse_ deleteGenreIfOrphan  genres