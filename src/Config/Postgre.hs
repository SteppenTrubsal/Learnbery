module Config.Postgre where

import           Control.Lens
import           Data.Aeson.TH
import           Data.Maybe (fromMaybe)
import           Database.PostgreSQL.Simple

data PostgreConf = PostgreConf
  { _host   :: Maybe String
  , _port   :: Maybe Int
  , _user   :: Maybe String
  , _passwd :: Maybe String
  }
makeLenses ''PostgreConf
$(deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''PostgreConf)

defPostgreConf :: PostgreConf -> ConnectInfo
defPostgreConf pc =
  defaultConnectInfo
    { connectHost = fromMaybe (connectHost defaultConnectInfo) $ pc ^. host
    , connectPort = maybe (connectPort defaultConnectInfo) fromIntegral (pc ^. port)
    , connectUser = fromMaybe (connectUser defaultConnectInfo) $ pc ^. user
    , connectPassword = fromMaybe (connectPassword defaultConnectInfo) $ pc ^. passwd
    }