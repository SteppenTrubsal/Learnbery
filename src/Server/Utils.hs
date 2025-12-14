module Server.Utils where

import           Control.Monad
import           Control.Monad.Reader

import           Data.Aeson
import           Data.ByteString.Char8
import           Data.Int                (Int32)
import           Data.Pool
import           Data.Text               (Text)
import           Data.Text.Encoding
import qualified Data.Text.Lazy.Encoding as TLE
import           Database.Beam.Postgres

import           Lucid

import           Network.HTTP.Types
import           Network.Wai

import           Server.Core


qParam :: Request -> ByteString -> Maybe ByteString
qParam req k = join (lookup k (queryString req))

qInt :: Request -> ByteString -> Maybe Int
qInt req k = qParam req k >>= (fmap fst . readInt)

qInt32 :: Request -> ByteString -> Maybe Int32
qInt32 req k = fromIntegral <$> qInt req k

qText :: Request -> ByteString -> Maybe Text
qText req k =
  case join (lookup k (queryString req)) of
    Nothing -> Nothing
    Just bs -> Just (decodeUtf8 bs)

jsonResp :: Status -> Response
jsonResp st = responseLBS st [("Content-Type","application/json; charset=utf-8")] ""

jsonRespBody :: Status -> Text -> Response
jsonRespBody st body = responseLBS st [("Content-Type","application/json; charset=utf-8")] (encode (object ["error" .= body]))

respondJson :: ToJSON a => (Response -> IO ResponseReceived) -> Status -> a -> IO ResponseReceived
respondJson res st a =
  res $ responseLBS st [("Content-Type","application/json; charset=utf-8")] (encode a)

pageResponse :: Status -> Html () -> Response
pageResponse st pg = responseLBS st [("Content-Type","text/html; charset=utf-8")]
                (TLE.encodeUtf8 $ renderText pg)

runQuery :: Pg a -> App a
runQuery q = do
  pool <- asks bdPool
  liftIO $
    withResource pool $ \conn ->
      runBeamPostgres conn q