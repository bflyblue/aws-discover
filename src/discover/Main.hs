{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified AWS.CloudFormation.Stacks
import qualified AWS.EC2.Instances
import qualified AWS.EC2.SecurityGroups
import qualified AWS.EC2.Subnets
import qualified AWS.EC2.Vpcs
import qualified AWS.Lambda.Functions
import qualified AWS.Lambda.GetFunctions
import qualified AWS.RDS.Instances
import qualified AWS.ResourceGroupsTagging.Resources
import qualified AWS.SecretsManager.Secrets
import qualified Amazonka
import Config (readConfigFile)
import Control.Concurrent.Async
import Control.Lens ((^..))
import Control.Monad (when)
import Data.Aeson (Value (Array))
import Data.Aeson.Lens (key, values)
import Data.Foldable (Foldable (toList), for_, traverse_)
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

  -- Discover basic resources
  runConcurrently $
    traverse_ @[]
      Concurrently
      [ AWS.CloudFormation.Stacks.discover env cfg
      , AWS.EC2.Instances.discover env cfg
      , AWS.EC2.SecurityGroups.discover env cfg
      , AWS.EC2.Subnets.discover env cfg
      , AWS.EC2.Vpcs.discover env cfg
      , AWS.Lambda.Functions.discover env cfg
      , AWS.RDS.Instances.discover env cfg
      , AWS.ResourceGroupsTagging.Resources.discover env cfg
      ]

  -- Discover secrets that are referred to in environment variables
  -- TODO: only secrets for current owner (env)
  AWS.SecretsManager.Secrets.discover env cfg

  -- Create nodes for certain tags and link to resources
  Tags.discover cfg "app" "App" "App"
  Tags.discover cfg "env" "Environment" "Environment"
  Tags.discover cfg "team" "Team" "Team"
  Tags.discover cfg "product" "Product" "Product"
  Tags.discover cfg "service" "Service" "Service"
  Tags.discover cfg "system" "System" "System"

  -- Discover other relationships between resources
  runConcurrently $
    traverse_ @[]
      (Concurrently . withDb cfg . flip run)
      [ relateInVpc
      , relateHasSubnet
      , relateHasSecurityGroup
      , relateStackResources
      ]

  -- TODO: only lambdas for current owner (env)
  -- AWS.Lambda.GetFunctions.getFunctions env cfg

  return ()
 where
  relateInVpc = do
    eachMatchNode_ (hasLabel "Vpc") $ \v -> do
      for_ (getProperty "vpcId" (nodeProperties v)) $ \vpcId -> do
        eachMatchNode_ (hasLabel "Instance" .&. props .- "vpcId" .=. lit vpcId) $ \x ->
          mergeEdge_ ["InVpc"] (properties []) (nodeId x) (nodeId v)
        eachMatchNode_ (hasLabel "Lambda" .&. props .- "vpcConfig" .- "vpcId" .=. lit vpcId) $ \x ->
          mergeEdge_ ["InVpc"] (properties []) (nodeId x) (nodeId v)

  relateHasSubnet = do
    eachMatchNode_ (hasLabel "Subnet") $ \s -> do
      for_ (getProperty "subnetId" (nodeProperties s)) $ \subnetId -> do
        eachMatchNode_ (hasLabel "Lambda" .&. props .- "vpcConfig" .- "subnetIds" .@>. lit subnetId) $ \x ->
          mergeEdge_ ["HasSubnet"] (properties []) (nodeId x) (nodeId s)
        eachMatchNode_ (hasLabel "Instance" .&. props .- "subnetId" .=. lit subnetId) $ \x ->
          mergeEdge_ ["HasSubnet"] (properties []) (nodeId x) (nodeId s)
      for_ (getProperty "vpcId" (nodeProperties s)) $ \vpcId -> do
        eachMatchNode_ (hasLabel "Vpc" .&. props .- "vpcId" .=. lit vpcId) $ \x ->
          mergeEdge_ ["HasSubnet"] (properties []) (nodeId x) (nodeId s)

  relateHasSecurityGroup = do
    eachMatchNode_ (hasLabel "Instance") $ \i -> do
      let groups = fromProperties (nodeProperties i) ^.. key "securityGroups" . values . key "groupId"
      for_ groups $ \groupId -> do
        eachMatchNode_ (hasLabel "SecurityGroup" .&. props .- "groupId" .=. lit groupId) $ \g -> do
          mergeEdge_ ["HasSecurityGroup"] (properties []) (nodeId i) (nodeId g)

  relateStackResources = do
    eachMatchNode_ (hasLabel "Stack") $ \s -> do
      eachMatchEdge_ (hasLabel "HasStackResource") (Just $ nodeId s) Nothing $ \e -> do
        sr <- getNode (edgeB e)
        let prop = nodeProperties sr
        when ("StackResource" `labelIn` nodeLabels sr) $ do
          for_ (getProperty "physicalResourceId" prop) $ \resourceId -> do
            for_ (getProperty "region" prop) $ \region -> do
              case getProperty "resourceType" prop of
                Just "AWS::Lambda::Function" -> do
                  eachMatchNode_
                    (hasLabel "Lambda" .&. props .- "region" .=. lit region .&. props .- "functionName" .=. lit resourceId)
                    $ \r -> do
                      mergeEdge_ ["HasResource"] (nodeProperties sr) (nodeId s) (nodeId r)
                _ -> do
                  return ()

eachMatchNode_ :: Match Bool -> (Node -> Session.Session a) -> Session.Session ()
eachMatchNode_ p act = matchNode p >>= mapM_ act

eachMatchEdge_ :: Match Bool -> Maybe (Id Node) -> Maybe (Id Node) -> (Edge -> Session.Session a) -> Session.Session ()
eachMatchEdge_ p a b act = matchEdge p a b >>= mapM_ act

array :: (Value -> t) -> ([Value] -> t) -> Value -> t
array _ f (Array a) = f (toList a)
array g _ x = g x

fromArray :: t -> ([Value] -> t) -> Value -> t
fromArray d = array (const d)