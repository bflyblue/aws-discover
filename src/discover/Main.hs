{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified AWS.EC2.Instances
import qualified AWS.EC2.SecurityGroups
import qualified AWS.EC2.Subnets
import qualified AWS.EC2.Vpcs
import qualified AWS.Lambda.Functions
import qualified AWS.RDS.Instances
import qualified AWS.ResourceGroupsTagging.Resources
import qualified AWS.SecretsManager.Secrets
import qualified Amazonka
import Config (readConfigFile)
import Control.Concurrent.Async
import Control.Lens ((^..))
import Control.Monad (forM_)
import Data.Aeson (Value (Array))
import Data.Aeson.Lens (key, values)
import Data.Foldable (Foldable (toList), traverse_)
import Database
import qualified Hasql.Session as Session
import System.IO (stdout)
import qualified Tags

main :: IO ()
main = do
  cfg <- readConfigFile "aws-discover.yaml"
  lgr <- Amazonka.newLogger Amazonka.Info stdout
  discoveredEnv <- Amazonka.newEnv Amazonka.discover
  let env =
        discoveredEnv
          { Amazonka.logger = lgr
          , Amazonka.region = Amazonka.Ireland
          }

  runConcurrently $
    traverse_ @[]
      Concurrently
      [ AWS.EC2.Instances.discover env cfg
      , AWS.EC2.SecurityGroups.discover env cfg
      , AWS.EC2.Subnets.discover env cfg
      , AWS.EC2.Vpcs.discover env cfg
      , AWS.Lambda.Functions.discover env cfg
      , AWS.RDS.Instances.discover env cfg
      , AWS.ResourceGroupsTagging.Resources.discover env cfg
      ]

  AWS.SecretsManager.Secrets.discover env cfg
  Tags.discover cfg "app" "App" "App"
  Tags.discover cfg "env" "Environment" "Environment"
  Tags.discover cfg "team" "Team" "Team"
  Tags.discover cfg "product" "Product" "Product"
  Tags.discover cfg "service" "Service" "Service"
  Tags.discover cfg "system" "System" "System"

  runConcurrently $
    traverse_ @[]
      (Concurrently . withDb cfg . flip run)
      [ relateInVpc
      , relateHasSubnet
      , relateHasSecurityGroup
      ]
 where
  relateInVpc = do
    eachMatchNode_ (hasLabel "Vpc") $ \v -> do
      forM_ (getProperty "vpcId" (nodeProperties v)) $ \vpcId -> do
        eachMatchNode_ (hasLabel "Instance" .&. props .- "vpcId" .=. lit vpcId) $ \x ->
          mergeEdge_ ["InVpc"] (properties []) (nodeId x) (nodeId v)
        eachMatchNode_ (hasLabel "Lambda" .&. props .- "vpcConfig" .- "vpcId" .=. lit vpcId) $ \x ->
          mergeEdge_ ["InVpc"] (properties []) (nodeId x) (nodeId v)

  relateHasSubnet = do
    eachMatchNode_ (hasLabel "Subnet") $ \s -> do
      forM_ (getProperty "subnetId" (nodeProperties s)) $ \subnetId -> do
        eachMatchNode_ (hasLabel "Lambda" .&. props .- "vpcConfig" .- "subnetIds" .@>. lit subnetId) $ \x ->
          mergeEdge_ ["HasSubnet"] (properties []) (nodeId x) (nodeId s)
        eachMatchNode_ (hasLabel "Instance" .&. props .- "subnetId" .=. lit subnetId) $ \x ->
          mergeEdge_ ["HasSubnet"] (properties []) (nodeId x) (nodeId s)
      forM_ (getProperty "vpcId" (nodeProperties s)) $ \vpcId -> do
        eachMatchNode_ (hasLabel "Vpc" .&. props .- "vpcId" .=. lit vpcId) $ \x ->
          mergeEdge_ ["HasSubnet"] (properties []) (nodeId x) (nodeId s)

  relateHasSecurityGroup = do
    eachMatchNode_ (hasLabel "Instance") $ \i -> do
      let groups = fromProperties (nodeProperties i) ^.. key "securityGroups" . values . key "groupId"
      forM_ groups $ \groupId -> do
        eachMatchNode_ (hasLabel "SecurityGroup" .&. props .- "groupId" .=. lit groupId) $ \g -> do
          mergeEdge_ ["HasSecurityGroup"] (properties []) (nodeId i) (nodeId g)

eachMatchNode_ :: Match Bool -> (Node -> Session.Session a) -> Session.Session ()
eachMatchNode_ p a = matchNode p >>= mapM_ a

array :: (Value -> t) -> ([Value] -> t) -> Value -> t
array _ f (Array a) = f (toList a)
array g _ x = g x

fromArray :: t -> ([Value] -> t) -> Value -> t
fromArray d = array (const d)