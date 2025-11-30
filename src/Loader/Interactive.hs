module Loader.Interactive where

import           Data.Int       (Int32)
import qualified Data.Text      as T
import           Data.Text      (Text)
import qualified Data.Text.Read as TR

import           Loader.Types

parseInt32 :: Text -> Maybe Int32
parseInt32 t =
  case TR.decimal (T.strip t) of
    Right (n, _) -> Just n
    Left _       -> Nothing

splitCommaList :: Text -> [Text]
splitCommaList =
  map T.strip . filter (not . T.null) . T.splitOn ","

parseBookLine :: Text -> Either Text BookInput
parseBookLine line =
  case map T.strip (T.splitOn "|" line) of
    [t, d, y, p, as, gs] ->
      case (parseInt32 y, parseInt32 p) of
        (Just y', Just p') ->
          Right $ BookInput
            { biTitle   = t
            , biDesc    = d
            , biYear    = y'
            , biPages   = p'
            , biAuthors = splitCommaList as
            , biGenres  = splitCommaList gs
            }
        _ -> Left "Failed to parse year/pages as Int32"
    _ ->
      Left "I expect 6 fields separated by ‘|’: title | desc | year | pages | authors | genres"
