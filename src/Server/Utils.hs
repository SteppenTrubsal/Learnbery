module Server.Utils where

import           Control.Monad.Reader

import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import           Data.Pool
import           Data.Text            (Text)
import           Data.Text.Lazy.Encoding
import           Database.Beam.Postgres

import           Lucid

import           Network.HTTP.Types
import           Network.Wai

import           Server.Core

htmlResponse :: ByteString -> Response
htmlResponse = responseLBS status200 [("Content-Type","text/html; charset=utf-8")]

pageResponse :: Status -> Html () -> Response
pageResponse st pg = responseLBS st [("Content-Type","text/html; charset=utf-8")]
                (encodeUtf8 $ renderText pg) 

redirectTo :: ByteString -> Response
redirectTo loc = responseLBS status302 [(hLocation, BS.toStrict loc)] ""

isStatic :: [Text] -> Bool
isStatic [] = False
isStatic (x : _) = x == "css" || x == "js" || x == "icon" || x == "images" || x == "favicon.ico"

runQuery :: Pg a -> App a
runQuery q = do
  pool <- asks bdPool
  liftIO $
    withResource pool $ \conn ->
      runBeamPostgres conn q