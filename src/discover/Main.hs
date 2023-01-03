{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Amazonka
import Data.Time.Clock (getCurrentTime)
import System.IO (stdout)

import qualified AWS.EC2.Instances
import qualified AWS.EC2.Subnets
import qualified AWS.EC2.Vpcs
import Config (readConfigFile)
import Control.Monad (forM_)
import Data.Aeson (KeyValue (..))
import qualified Data.Aeson.KeyMap as KeyMap
import Database

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

  withDb cfg $ \conn ->
    run conn $ do
      is <- matchNode (hasLabel "Instance")
      forM_ is $ \i -> do
        vs <- matchNode (hasLabel "Vpc" .&. hasProperties (properties ["vpcId" .= KeyMap.lookup "vpcId" (unProperties $ nodeProperties i)]))
        forM_ vs $ \v -> do
          _ <- mergeEdge ["InVpc"] (properties []) (nodeId i) (nodeId v)
          return ()