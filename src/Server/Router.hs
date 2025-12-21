module Server.Router where

import           Control.Lens          ((^.))
import           Control.Monad.Reader
import           Control.Monad

import           Data.Aeson
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import           Data.Int
import           Data.Maybe

import           Network.Wai
import           Network.HTTP.Types

import           System.FilePath

import           Text.Read

import           HTML.Error
import           HTML.Main
import           Config
import           Config.Common
import           Server.Core
import           Server.Utils
import           Storage.Queries
import           Storage.Schema
import           Storage.Types
import           Storage.Service

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

    ["r", "book", bid, "download"] -> downloadEndpoint bid res

    ["fortune"]  -> fortuneEndpoint res

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

  conf <- asks config
  let resourceDir = conf ^. common . resDir

  payload <- liftIO $ forM books $ \(BookT bid title _ _ _) -> do
    let
      bidStr  = show bid
      bookDir = resourceDir </> bidStr
    files <- liftIO $ safeListDir bookDir

    let
      mPic  = findFirstByPrefix "cover." files
      mBook = findFirstByPrefix "file." files

      coverUrl =
        case mPic of
          Just picFile -> T.pack (bidStr </> picFile)
          Nothing      -> "https://placehold.co/200x300/cccccc/ffffff?text=" <> title

      downloadUrl =
        case mBook of
          Just _        -> Just (T.pack ("/r/book/" <> bidStr <> "/download"))
          Nothing       -> Nothing
    pure $ object
      [ "id"          .= bid
      , "title"       .= title
      , "cover"       .= coverUrl
      , "downloadUrl" .= downloadUrl
      ]
  liftIO $ respondJson res status200 payload

getDownloadUrl :: Int32 -> App (Maybe Text)
getDownloadUrl bid = do
  conf <- asks config
  let
    resourceDir = conf ^. common . resDir
    bidStr  = show bid
    bookDir = resourceDir </> bidStr
  
  files <- liftIO $ safeListDir bookDir

  let 
    mBook = findFirstByPrefix "file." files
    downloadUrl =
      case mBook of
        Just _        -> Just (T.pack ("/r/book/" <> bidStr <> "/download"))
        Nothing       -> Nothing
  pure downloadUrl


bookDetailsEndpointText :: Text -> (Response -> IO ResponseReceived) -> App ResponseReceived
bookDetailsEndpointText bidTxt res =
  case readMaybe (T.unpack bidTxt) :: Maybe Int of
    Nothing -> liftIO $ respondJson res status400 (object ["error" .= ("Invalid id" :: Text)])
    Just bidInt -> do
      let bid = fromIntegral bidInt :: Int32
      mfb <- runQuery $ selectFullBookById bid
      case mfb of
        Nothing -> liftIO $ respondJson res status404 (object ["error" .= ("Not found" :: Text)])
        Just (FullBook (BookT _ title desc year pages) authors genres) -> do
          let 
            authorNames = map _authorName authors
            genreNames  = map _genreName  genres
          mDownloadUrl <- getDownloadUrl bid
          let downloadUrl =  fromMaybe "null" mDownloadUrl
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
              , "downloadUrl" .= downloadUrl
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

downloadEndpoint :: Text -> (Response -> IO ResponseReceived) -> App ResponseReceived
downloadEndpoint bid res = do
  case readMaybe (T.unpack bid) of
    Nothing -> liftIO $ respondJson res status400 (object ["error" .= ("Invalid id" :: T.Text)])
    Just bidInt -> do
      conf <- asks config
      let
        resourcesDir = conf ^. common . resDir
        bidStr = show bidInt
        dir = resourcesDir </> bidStr

      files <- liftIO $ safeListDir dir
      case findFirstByPrefix "file." files of
        Nothing -> liftIO $ respondJson res status404 (object ["error" .= ("book.* not found" :: T.Text)])
        Just bookFile -> do
          mTitle <- runQuery $ selectTitleById bidInt
          let
            fullPath = dir </> bookFile
            title = fromMaybe "undefined" mTitle <> T.pack (takeExtension bookFile)
          liftIO $ res $
            responseFile
              status200
              [ ("Content-Type", "application/octet-stream")
              , ("Content-Disposition", TE.encodeUtf8 ("attachment; filename=\"" <> title <> "\""))
              ]
              fullPath
              Nothing

fortuneEndpoint :: (Response -> IO ResponseReceived) -> App ResponseReceived
fortuneEndpoint res = do
  books <- runQuery $ selectBooks

  let 
    payload =
      map (\(BookT bid title _ _ _) ->
        object
          [ "id"    .= bid
          , "title" .= title
          ]
      ) books
  
  liftIO $ respondJson res status200 payload