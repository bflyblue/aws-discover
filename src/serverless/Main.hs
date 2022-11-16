{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Main where

import Config
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Yaml ((.:))
import qualified Data.Yaml as Yaml
import Database

data Serverless = Serverless
  { service :: Text
  , provider :: Provider
  , functions :: HashMap Text Function
  }
  deriving (Show)

data Provider = Provider
  { name :: Text
  , runtime :: Text
  , apiGateway :: ApiGateway
  , region :: Text
  }
  deriving (Show)

data ApiGateway = ApiGateway
  { shouldStartNameWithService :: Bool
  }
  deriving (Show)

data Function = Function
  { handler :: Text
  }
  deriving (Show)

instance Yaml.FromJSON Serverless where
  parseJSON = Yaml.withObject "serverless" $ \o ->
    Serverless
      <$> o
      .: "service"
      <*> o
      .: "provider"
      <*> o
      .: "functions"

instance Yaml.FromJSON Provider where
  parseJSON = Yaml.withObject "provider" $ \o ->
    Provider
      <$> o
      .: "name"
      <*> o
      .: "runtime"
      <*> o
      .: "apiGateway"
      <*> o
      .: "region"

instance Yaml.FromJSON ApiGateway where
  parseJSON = Yaml.withObject "apiGateway" $ \o ->
    ApiGateway
      <$> o
      .: "shouldStartNameWithService"

instance Yaml.FromJSON Function where
  parseJSON = Yaml.withObject "function" $ \o ->
    Function
      <$> o
      .: "handler"

main :: IO ()
main = do
  cfg <- readConfigFile "aws-discover.yaml"
  sls <- Yaml.decodeFileThrow "serverless.yml"
  print (sls :: Serverless)
