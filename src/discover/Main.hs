{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified AWS.EC2.Instances
import qualified AWS.EC2.Subnets
import qualified AWS.EC2.Vpcs
import qualified AWS.Lambda.Functions
import qualified Amazonka
import Config (readConfigFile)
import Control.Concurrent.Async
import Control.Monad (forM_, void)
import Data.Aeson (KeyValue (..), Value (Array), object)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Foldable (Foldable (toList), sequenceA_)
import Data.Time.Clock (getCurrentTime)
import Database
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

  runConcurrently $
    sequenceA_ @[]
      [ Concurrently (AWS.EC2.Instances.discover env cfg now)
      , Concurrently (AWS.EC2.Vpcs.discover env cfg now)
      , Concurrently (AWS.EC2.Subnets.discover env cfg now)
      , Concurrently (AWS.Lambda.Functions.discover env cfg now)
      ]

  withDb cfg $ \pool ->
    run pool $ do
      vs <- matchNode (hasLabel "Vpc")
      forM_ vs $ \v -> do
        let vpcId = KeyMap.lookup "vpcId" (unProperties $ nodeProperties v)
        matchNode
          (hasLabel "Instance" .&. hasProperties (properties ["vpcId" .= vpcId]))
          >>= mapM_ (\x -> void $ mergeEdge ["InVpc"] (properties []) (nodeId x) (nodeId v))
        matchNode
          (hasLabel "Lambda" .&. hasProperties (properties ["vpcConfig" .= object ["vpcId" .= vpcId]]))
          >>= mapM_ (\x -> void $ mergeEdge ["InVpc"] (properties []) (nodeId x) (nodeId v))
        matchNode
          (hasLabel "Subnet" .&. hasProperties (properties ["vpcId" .= vpcId]))
          >>= mapM_ (\x -> void $ mergeEdge ["HasSubnet"] (properties []) (nodeId v) (nodeId x))
      ss <- matchNode (hasLabel "Subnet")
      forM_ ss $ \s -> do
        case KeyMap.lookup "subnetId" (unProperties $ nodeProperties s) of
          Just subnetId -> do
            matchNode
              (hasLabel "Lambda" .&. subnetId `propertyElemOf` "subnetIds")
              >>= mapM_ (\x -> void $ mergeEdge ["HasSubnet"] (properties []) (nodeId x) (nodeId s))
          Nothing -> return ()

array :: (Value -> t) -> ([Value] -> t) -> Value -> t
array _ f (Array a) = f (toList a)
array g _ x = g x

fromArray :: t -> ([Value] -> t) -> Value -> t
fromArray d = array (const d)