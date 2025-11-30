module Loader.Queries where

import           Data.Text              (Text)
import           Database.Beam
import           Database.Beam.Postgres

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
             filter_ (\b ->
               (_title b ==. val_ (biTitle bi)) &&.
               (_desc  b ==. val_ (biDesc bi))  &&.
               (_year  b ==. val_ (biYear bi))  &&.
               (_pages b ==. val_ (biPages bi))
             ) $
             all_ (_books libraryDb)

  book <- maybe (fail "insertBookWithRelations: can't find inserted book")
                pure
                mBook

  let bookIdPk = BookId (_bookId book)

  runInsert $
    insert (_bookAuthors libraryDb) $
      insertValues
        [ BookAuthorT bookIdPk authorId | authorId <- authorIds ]

  runInsert $
    insert (_bookGenres libraryDb) $
      insertValues
        [ BookGenreT bookIdPk genreId | genreId <- genreIds ]

  pure ()