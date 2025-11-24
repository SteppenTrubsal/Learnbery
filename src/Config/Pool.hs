module Config.Pool where

import           Control.Lens  (makeLenses)
import           Data.Aeson.TH

data PoolConf = PoolConf
  { _keepAlive    :: Double
  , _resPerStripe :: Int
  }
makeLenses ''PoolConf
$(deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''PoolConf)