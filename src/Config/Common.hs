module Config.Common where

import           Control.Lens  (makeLenses)
import           Data.Aeson.TH

data CommonConf = CommonConf
  { _port    :: Int
  , _siteDir :: FilePath
  , _resDir  :: FilePath
  }
makeLenses ''CommonConf
$(deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''CommonConf)