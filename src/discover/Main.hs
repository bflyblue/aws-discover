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
import Data.Foldable (Foldable (toList), sequenceA_)
import Database
import qualified Hasql.Session as Session
import System.IO (stdout)

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
    sequenceA_ @[]
      [ Concurrently (AWS.EC2.Instances.discover env cfg)
      , Concurrently (AWS.EC2.SecurityGroups.discover env cfg)
      , Concurrently (AWS.EC2.Subnets.discover env cfg)
      , Concurrently (AWS.EC2.Vpcs.discover env cfg)
      , Concurrently (AWS.Lambda.Functions.discover env cfg)
      , Concurrently (AWS.RDS.Instances.discover env cfg)
      , Concurrently (AWS.ResourceGroupsTagging.Resources.discover env cfg)
      ]

  AWS.SecretsManager.Secrets.discover env cfg

  withDb cfg $ \pool ->
    run pool $ do
      eachMatchNode_ (hasLabel "Vpc") $ \v -> do
        forM_ (getProperty "vpcId" (nodeProperties v)) $ \vpcId -> do
          eachMatchNode_ (hasLabel "Instance" .&. props .- "vpcId" .=. lit vpcId) $ \x ->
            mergeEdge_ ["InVpc"] (properties []) (nodeId x) (nodeId v)
          eachMatchNode_ (hasLabel "Lambda" .&. props .- "vpcConfig" .- "vpcId" .=. lit vpcId) $ \x ->
            mergeEdge_ ["InVpc"] (properties []) (nodeId x) (nodeId v)
          eachMatchNode_ (hasLabel "Subnet" .&. props .- "vpcId" .=. lit vpcId) $ \x ->
            mergeEdge_ ["HasSubnet"] (properties []) (nodeId v) (nodeId x)

      eachMatchNode_ (hasLabel "Subnet") $ \s ->
        forM_ (getProperty "subnetId" (nodeProperties s)) $ \subnetId -> do
          eachMatchNode_ (hasLabel "Lambda" .&. props .- "vpcConfig" .- "subnetIds" .@>. lit subnetId) $ \x ->
            mergeEdge_ ["HasSubnet"] (properties []) (nodeId x) (nodeId s)
          eachMatchNode_ (hasLabel "Instance" .&. props .- "subnetId" .=. lit subnetId) $ \x ->
            mergeEdge_ ["HasSubnet"] (properties []) (nodeId x) (nodeId s)

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