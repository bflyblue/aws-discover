{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Config where

import Data.Text (Text)
import Data.Word (Word16)
import qualified Data.Yaml as Yaml
import GHC.Generics (Generic)

newtype Config = Config
  { database :: PostgresConfig
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Yaml.FromJSON)

data PostgresConfig = PostgresConfig
  { host :: Text
  , port :: Word16
  , user :: Text
  , password :: Text
  , name :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Yaml.FromJSON)

readConfigFile :: FilePath -> IO Config
readConfigFile = Yaml.decodeFileThrow