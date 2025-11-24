module Config.Pool where

import           Control.Lens  (makeLenses)
import           Data.Aeson.TH
import           Data.Default

data PoolConf = PoolConf
  { _stripes      :: Maybe Int
  , _keepAlive    :: Maybe Int
  , _resPerStripe :: Maybe Int
  }
makeLenses ''PoolConf
$(deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''PoolConf)

instance Default PoolConf where
  def = PoolConf
    (Just 1)
    (Just 30)
    (Just 10)