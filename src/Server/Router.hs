module Server.Router where

import           Control.Monad.IO.Class

import           Data.Aeson
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Char8  as BSC
import qualified Data.Text              as T

import           Network.Wai
import           Network.HTTP.Types

import           Text.Read

import           HTML.Error
import           HTML.Main
import           Server.Core
import           Server.Utils
import           Storage.Queries
import           Storage.Schema

router :: Request -> (Response -> IO ResponseReceived) -> App ResponseReceived
router req res =
  case requestMethod req of
    "GET"  -> getRouter req res
    _      -> liftIO $ res $ pageResponse status400 (errorPage "Undefined http method - what did you do to get this)) ?")

getRouter :: Request -> (Response -> IO ResponseReceived) -> App ResponseReceived
getRouter req res = do
  case rawPathInfo req of
    "/test" -> do
      fb <- runQuery selectFullBooks
      liftIO $ res $ pageResponse status200 $ dbTest fb
    "/api/books" -> pagedEndpoint req res
    p | Just bs <- BSC.stripPrefix (BSC.pack "/api/book/") p -> do
      -- liftIO $ putStrLn "testapi"
      bookInfoEndpoint bs res
    "/" ->
      liftIO $ res $ pageResponse status200 bookPage
    _   ->
      liftIO $ res $ pageResponse status404 (errorPage "Page not found - nothing to see there")

pagedEndpoint :: Request -> (Response -> IO ResponseReceived) -> App ResponseReceived
pagedEndpoint req res = do
  let
    params = queryString req
    offset = case lookup "offset" params of
      Just (Just bs) -> maybe 0 fst (BSC.readInt bs)
      _              -> 0
    limit  = case lookup "limit" params of
                    Just (Just bs) -> maybe 20 fst (BSC.readInt bs)
                    _              -> 20

  books <- runQuery $ selectBooksPaged offset limit

  let resultJSON = encode $ map (\b -> 
                         object [ "id"    .= _bookId b
                                , "title" .= _title b
                                , "cover" .= ( "https://placehold.co/200x300/cccccc/ffffff?text="
                                               <> T.replace " " "+" (_title b) )
                                ]) books

  liftIO $ res $ bsResponse resultJSON

bookInfoEndpoint :: ByteString -> (Response -> IO ResponseReceived) -> App ResponseReceived
bookInfoEndpoint bs res = do
  case readMaybe (BSC.unpack bs) :: Maybe Int of
    Nothing  -> 
      liftIO $ res $ bsResponse "{\"error\":\"Invalid book ID\"}"
    Just bid -> do
      mBook <- runQuery $ selectBookById (fromIntegral bid)
      case mBook of
        Nothing ->
          liftIO $ res $ bsResponse "{\"error\":\"Book not found\"}"
        Just (BookT _ title desc year pages) -> do
          authors <- runQuery $ selectAuthorsByBookId (fromIntegral bid)
          genres  <- runQuery $ selectGenresByBookId (fromIntegral bid)

          let detailJSON = object 
                    [ "id"      .= bid
                    , "title"   .= title
                    , "desc"    .= desc
                    , "year"    .= year
                    , "pages"   .= pages
                    , "authors" .= map _authorName authors
                    , "genres"  .= map _genreName genres
                    ]
          
          liftIO $ res $ bsResponse $ encode detailJSON