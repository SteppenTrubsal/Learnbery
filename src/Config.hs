module Config where

import           Control.Lens         (makeLenses)
import           Data.Aeson.TH
import qualified Data.Yaml            as Y
import           System.Exit          (die)

import           Config.Common
import           Config.Postgre

confPath :: FilePath
confPath = "server.yaml"

data Config = Config
  { _common  :: CommonConf
  , _postgre :: PostgreConf
  }
makeLenses ''Config
$(deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Config)

loadConf :: IO Config
loadConf = do
  eres <- Y.decodeFileEither confPath
  case eres of
    Left err  -> die (Y.prettyPrintParseException err)
    Right cfg -> pure cfg