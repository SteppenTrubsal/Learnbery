module Loader.Queries where

import           Control.Lens

import           Data.Text              (Text)
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