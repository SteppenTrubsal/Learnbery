module Server.Router where

import           Control.Monad.IO.Class

import           Data.Aeson
import qualified Data.Text              as T
import           Data.Int
import           Data.Maybe

import           Network.Wai
import           Network.HTTP.Types

import           Text.Read

import           HTML.Error
import           HTML.Main
import           Server.Core
import           Server.Utils
import           Storage.Queries
import           Storage.Schema
import           Storage.Types

router :: Request -> (Response -> IO ResponseReceived) -> App ResponseReceived
router req res =
  case requestMethod req of
    "GET" -> getRouter req res
    _     -> liftIO $ res $ pageResponse status400 (errorPage "Undefined http method - what did you do to get this)) ?")

getRouter :: Request -> (Response -> IO ResponseReceived) -> App ResponseReceived
getRouter req res = do
  case pathInfo req of
    ["api", "books"] -> booksEndpoint req res
    ["api", "books", bidTxt] -> bookDetailsEndpointText bidTxt res
    ["api", "catalog", "filters"] -> catalogFiltersEndpoint res

    [] -> liftIO $ res $ pageResponse status200 bookPage
    ["test"] -> do
      fb <- runQuery selectFullBooks
      liftIO $ res $ pageResponse status200 $ dbTest fb

    _ -> liftIO $ res $ pageResponse status404 (errorPage "Page not found - nothing to see there")

booksEndpoint :: Request -> (Response -> IO ResponseReceived) -> App ResponseReceived
booksEndpoint req res = do
  let off   = fromMaybe 0 (qInt req "offset")
      lim   = fromMaybe 20 (qInt req "limit")
      mq      = qText req "q"
      ma      = qInt32 req "author"
      mg      = qInt32 req "genre"
      mAuthorQ = qText req "author_q"
      mGenreQ  = qText req "genre_q"
      yFrom   = qInt32 req "year_from"
      yTo     = qInt32 req "year_to"

  books <- runQuery $ selectBooksPagedFiltered off lim mq ma mg mAuthorQ mGenreQ yFrom yTo

  let payload =
        map (\(BookT bid title _ _ _) ->
          object
            [ "id"    .= bid
            , "title" .= title
            , "cover" .= ("https://placehold.co/200x300/cccccc/ffffff?text=" <> title)
            ]
        ) books

  liftIO $ respondJson res status200 payload

bookDetailsEndpointText :: T.Text -> (Response -> IO ResponseReceived) -> App ResponseReceived
bookDetailsEndpointText bidTxt res =
  case readMaybe (T.unpack bidTxt) :: Maybe Int of
    Nothing -> liftIO $ respondJson res status400 (object ["error" .= ("Invalid id" :: T.Text)])
    Just bidInt -> do
      let bid = fromIntegral bidInt :: Int32
      mfb <- runQuery $ selectFullBookById bid
      case mfb of
        Nothing -> liftIO $ respondJson res status404 (object ["error" .= ("Not found" :: T.Text)])
        Just (FullBook (BookT _ title desc year pages) authors genres) -> do
          let authorNames = map _authorName authors
              genreNames  = map _genreName  genres
          liftIO $ respondJson res status200 $
            object
              [ "id"      .= bidInt
              , "title"   .= title
              , "desc"    .= desc
              , "year"    .= year
              , "pages"   .= pages
              , "author"  .= (if null authorNames then ("Неизвестный автор" :: T.Text) else T.intercalate ", " authorNames)
              , "authors" .= authorNames
              , "genres"  .= genreNames
              ]

catalogFiltersEndpoint :: (Response -> IO ResponseReceived) -> App ResponseReceived
catalogFiltersEndpoint res = do
  authors <- runQuery selectAuthors
  genres  <- runQuery selectGenres
  years   <- runQuery selectBookYears

  let yMin = if null years then Nothing else Just (minimum years)
      yMax = if null years then Nothing else Just (maximum years)

  liftIO $ respondJson res status200 $
    object
      [ "authors" .= map (\(AuthorT aid nm) -> object ["id" .= aid, "name" .= nm]) authors
      , "genres"  .= map (\(GenreT  gid nm) -> object ["id" .= gid, "name" .= nm]) genres
      , "years"   .= object ["min" .= yMin, "max" .= yMax]
      ]

-- getRouter :: Request -> (Response -> IO ResponseReceived) -> App ResponseReceived
-- getRouter req res = do
--   case rawPathInfo req of
--     "/test" -> do
--       fb <- runQuery selectFullBooks
--       liftIO $ res $ pageResponse status200 $ dbTest fb
--     "/api/books" -> pagedEndpoint req res
--     p | Just bs <- BSC.stripPrefix (BSC.pack "/api/book/") p -> do
--       -- liftIO $ putStrLn "testapi"
--       bookInfoEndpoint bs res
--     "/" ->
--       liftIO $ res $ pageResponse status200 bookPage
--     _   ->
--       liftIO $ res $ pageResponse status404 (errorPage "Page not found - nothing to see there")

-- pagedEndpoint :: Request -> (Response -> IO ResponseReceived) -> App ResponseReceived
-- pagedEndpoint req res = do
--   let
--     params = queryString req
--     offset = case lookup "offset" params of
--       Just (Just bs) -> maybe 0 fst (BSC.readInt bs)
--       _              -> 0
--     limit  = case lookup "limit" params of
--                     Just (Just bs) -> maybe 20 fst (BSC.readInt bs)
--                     _              -> 20

--   books <- runQuery $ selectBooksPaged offset limit

--   let resultJSON = encode $ map (\b -> 
--                          object [ "id"    .= _bookId b
--                                 , "title" .= _title b
--                                 , "cover" .= ( "https://placehold.co/200x300/cccccc/ffffff?text="
--                                                <> T.replace " " "+" (_title b) )
--                                 ]) books

--   liftIO $ res $ bsResponse resultJSON

-- bookInfoEndpoint :: BSC.ByteString -> (Response -> IO ResponseReceived) -> App ResponseReceived
-- bookInfoEndpoint bs res =
--   case BSC.readInt bs of
--     Nothing -> liftIO $ res $ responseJson status400 (object ["error" .= ("Invalid book id" :: T.Text)])
--     Just (bidInt, _) -> do
--       let bid = fromIntegral bidInt
--       mfb <- runQuery (selectFullBookById bid)

--       case mfb of
--         Nothing -> liftIO $ res $ responseJson status404 (object ["error" .= ("Book not found" :: T.Text)])
--         Just (FullBook (BookT _ title desc year pages) authors genres) -> do
--           let authorNames = map _authorName authors
--               genreNames  = map _genreName  genres
--               authorText  = if null authorNames then ("Неизвестный автор" :: T.Text) else T.intercalate ", " authorNames

--           liftIO $ res $ responseJson status200 $
--             object
--               [ "id"      .= bidInt
--               , "title"   .= title
--               , "desc"    .= desc
--               , "year"    .= year
--               , "pages"   .= pages
--               , "author"  .= authorText
--               , "authors" .= authorNames
--               , "genres"  .= genreNames
--               ]
--   where
--     responseJson st v =
--       responseLBS st [("Content-Type","application/json; charset=utf-8")] $ encode v