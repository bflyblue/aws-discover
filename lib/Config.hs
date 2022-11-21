{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Config where

import Data.Text (Text)
import qualified Data.Yaml as Yaml
import GHC.Generics (Generic)

data Config = Config
  { host :: String
  , user :: Text
  , password :: Text
  , secure :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Yaml.FromJSON)

readConfigFile :: FilePath -> IO Config
readConfigFile = Yaml.decodeFileThrow