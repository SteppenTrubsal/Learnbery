module Config.Pool where

import           Control.Lens  (makeLenses)
import           Data.Aeson.TH

data PoolConf = PoolConf
  { _stripes      :: Int
  , _keepAlive    :: Int
  , _resPerStripe :: Int
  }
makeLenses ''PoolConf
$(deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''PoolConf)