module Config.Postgre where

import           Control.Lens  (makeLenses)
import           Data.Aeson.TH

data PostgreConf = PostgreConf
  { _host   :: Maybe String
  , _port   :: Maybe Int
  , _user   :: Maybe String
  , _passwd :: Maybe String 
  }
makeLenses ''PostgreConf
$(deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''PostgreConf)