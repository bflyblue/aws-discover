{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified AWS.EC2.Instances
import qualified AWS.EC2.Subnets
import qualified AWS.EC2.Vpcs
import qualified AWS.Lambda.Functions
import Config (readConfigFile)
import Database

import qualified Amazonka
import Control.Monad (forM_, void)
import Data.Aeson (KeyValue (..), object)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Time.Clock (getCurrentTime)
import System.IO (stdout)

main :: IO ()
main = do
  cfg <- readConfigFile "aws-discover.yaml"
  lgr <- Amazonka.newLogger Amazonka.Info stdout
  discoveredEnv <- Amazonka.newEnv Amazonka.discover
  let env =
        discoveredEnv
          { Amazonka.envLogger = lgr
          , Amazonka.envRegion = Amazonka.Ireland
          }

  now <- getCurrentTime

  AWS.EC2.Instances.discover env cfg now
  AWS.EC2.Vpcs.discover env cfg now
  AWS.EC2.Subnets.discover env cfg now
  AWS.Lambda.Functions.discover env cfg now

  withDb cfg $ \pool ->
    run pool $ do
      vs <- matchNode (hasLabel "Vpc")
      forM_ vs $ \v -> do
        let vpcId = KeyMap.lookup "vpcId" (unProperties $ nodeProperties v)
        matchNode
          ( hasLabel "Instance" .&. hasProperties (properties ["vpcId" .= vpcId])
          )
          >>= mapM_ (\x -> void $ mergeEdge ["InVpc"] (properties []) (nodeId x) (nodeId v))
        matchNode
          ( hasLabel "Lambda" .&. hasProperties (properties ["vpcConfig" .= object ["vpcId" .= vpcId]])
          )
          >>= mapM_ (\x -> void $ mergeEdge ["InVpc"] (properties []) (nodeId x) (nodeId v))