{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import qualified Amazonka as AWS
import qualified Amazonka.EC2 as EC2
import qualified Amazonka.EC2.DescribeInstances as EC2
import qualified Amazonka.EC2.DescribeSubnets as EC2
import qualified Amazonka.EC2.DescribeVpcs as EC2
import qualified Amazonka.EC2.Types.Instance as Inst
import qualified Amazonka.EC2.Types.InstanceState as InstState
import qualified Amazonka.EC2.Types.Reservation as Reservation
import qualified Amazonka.EC2.Types.Subnet as Subnet
import qualified Amazonka.EC2.Types.Vpc as Vpc
import qualified Amazonka.EC2.Types.VpcCidrBlockAssociation as VpcCidr
import qualified Amazonka.EC2.Types.VpcIpv6CidrBlockAssociation as VpcCidr
import qualified Amazonka.Lambda as Lambda
import qualified Amazonka.Lambda.ListFunctions as Lambda
import qualified Amazonka.Lambda.Types.FunctionConfiguration as FuncCfg
import qualified Amazonka.Lambda.Types.VpcConfigResponse as VpcCfg
import Conduit
import Control.Concurrent.Async (mapConcurrently_)
import Control.Monad.Trans.Resource
import Data.Default
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Yaml as Yaml
import Database.Bolt ((=:))
import qualified Database.Bolt as Bolt
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import System.IO (stdout)

data Config = Config
  { host :: String
  , user :: Text
  , password :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Yaml.FromJSON)

main :: IO ()
main = do
  cfg <- Yaml.decodeFileThrow "aws-discover.yaml"
  lgr <- AWS.newLogger AWS.Info stdout
  discoveredEnv <- AWS.newEnv AWS.discover
  let env =
        discoveredEnv
          { AWS.envLogger = lgr
          , AWS.envRegion = AWS.Ireland
          }
      boltcfg =
        def
          { Bolt.host = host cfg
          , Bolt.user = user cfg
          , Bolt.password = password cfg
          }
  discover env boltcfg

withBolt :: MonadResource m => Bolt.BoltCfg -> (Bolt.Pipe -> m a) -> m a
withBolt cfg f = do
  (key, db) <- allocate (Bolt.connect cfg) Bolt.close
  r <- f db
  release key
  return r

discover :: AWS.Env -> Bolt.BoltCfg -> IO ()
discover env boltcfg =
  do
    runResourceT $ withBolt boltcfg $ \db -> Bolt.run db $ do
      Bolt.query_ "MATCH (i:Instance) DETACH DELETE i"
      Bolt.query_ "MATCH (v:Vpc) DETACH DELETE v"
      Bolt.query_ "MATCH (c:Cidr) DETACH DELETE c"
      Bolt.query_ "MATCH (c:Ipv6Cidr) DETACH DELETE c"
      Bolt.query_ "MATCH (s:Subnet) DETACH DELETE s"
      Bolt.query_ "MATCH (t:Tags) DETACH DELETE t"
      Bolt.query_ "MATCH (l:Lambda) DETACH DELETE l"
    mapConcurrently_
      run
      [ \db -> fetchAllEc2Instances env .| ingestInstances db
      , \db -> fetchAllSubnets env .| ingestSubnets db
      , \db -> fetchAllVpcs env .| ingestVpcs db
      , \db -> fetchAllLambdas env .| ingestLambdas db
      ]
    runResourceT $ withBolt boltcfg $ \db -> Bolt.run db $ do
      Bolt.query_ "MATCH (i:Instance), (v:Vpc) WHERE i.vpcId = v.vpcId CREATE (i)-[:IN_VPC]->(v)"
      Bolt.query_ "MATCH (l:Lambda), (v:Vpc) WHERE l.vpcId = v.vpcId CREATE (l)-[:IN_VPC]->(v)"
      Bolt.query_ "MATCH (s:Subnet), (v:Vpc) WHERE s.vpcId = v.vpcId CREATE (v)-[:HAS_SUBNET]->(s)"
      Bolt.query_ "MATCH (l:Lambda), (s:Subnet) WHERE s.subnetId IN l.subnetIds MERGE (l)-[:HAS_SUBNET]->(s)"
 where
  run :: MonadUnliftIO m => (Bolt.Pipe -> ConduitT () Void (ResourceT m) a) -> m a
  run c = runResourceT $ withBolt boltcfg (runConduit . c)

fetchAllEc2Instances :: MonadResource m => AWS.Env -> ConduitM () EC2.Instance m ()
fetchAllEc2Instances env = AWS.paginate env EC2.newDescribeInstances .| concatMapC extractInstances

extractInstances :: EC2.DescribeInstancesResponse -> [EC2.Instance]
extractInstances = concat . mapMaybe Reservation.instances . concat . EC2.reservations

instance Bolt.IsValue Natural where
  toValue = Bolt.toValue . fromIntegral @Natural @Integer

instance Bolt.IsValue EC2.Instance where
  toValue Inst.Instance'{..} =
    Bolt.toValue $
      Map.fromList
        [ "instanceId" =: instanceId
        , "instanceType" =: EC2.fromInstanceType instanceType
        , "platformDetails" =: platformDetails
        , "privateDnsName" =: privateDnsName
        , "privateIpAddress" =: privateIpAddress
        , "publicDnsName" =: publicDnsName
        , "publicIpAddress" =: publicIpAddress
        , "state" =: EC2.fromInstanceStateName (InstState.name state)
        , "subnetId" =: subnetId
        , "vpcId" =: vpcId
        ]

newtype Tags = Tags [EC2.Tag]

instance Bolt.IsValue Tags where
  toValue (Tags tags) = Bolt.toValue $ tagMap tags
   where
    tagPair (EC2.Tag' k v) = k =: v
    tagMap = Map.fromList . map tagPair

ingestInstances :: MonadIO m => Bolt.Pipe -> ConduitT Inst.Instance o m ()
ingestInstances db = mapM_C ingestInstance
 where
  ingestInstance :: MonadIO m => EC2.Instance -> m ()
  ingestInstance inst =
    Bolt.run db $ do
      Bolt.queryP_ "CREATE (i:Instance $i)" (Bolt.props ["i" =: inst])
      traverse_
        ( \tags ->
            Bolt.queryP_
              "MATCH (i:Instance) WHERE i.instanceId = $i CREATE (i)-[:HAS_TAGS]->(t:Tags $t)"
              (Bolt.props ["i" =: Inst.instanceId inst, "t" =: Tags tags])
        )
        (Inst.tags inst)

fetchAllVpcs :: MonadResource m => AWS.Env -> ConduitM () EC2.Vpc m ()
fetchAllVpcs env = AWS.paginate env EC2.newDescribeVpcs .| concatMapC extractVpcs

extractVpcs :: EC2.DescribeVpcsResponse -> [EC2.Vpc]
extractVpcs = concat . EC2.vpcs

instance Bolt.IsValue EC2.Vpc where
  toValue Vpc.Vpc'{..} =
    Bolt.toValue $
      Map.fromList
        [ "ownerId" =: ownerId
        , "isDefault" =: isDefault
        , "state" =: EC2.fromVpcState state
        , "vpcId" =: vpcId
        ]

instance Bolt.IsValue EC2.VpcCidrBlockAssociation where
  toValue VpcCidr.VpcCidrBlockAssociation'{..} =
    Bolt.toValue $
      Map.fromList
        [ "cidrBlock" =: cidrBlock
        , "associationId" =: associationId
        ]

instance Bolt.IsValue EC2.VpcIpv6CidrBlockAssociation where
  toValue VpcCidr.VpcIpv6CidrBlockAssociation'{..} =
    Bolt.toValue $
      Map.fromList
        [ "networkBorderGroup" =: networkBorderGroup
        , "ipv6Pool" =: ipv6Pool
        , "ipv6CidrBlock" =: ipv6CidrBlock
        , "associationId" =: associationId
        ]

ingestVpcs :: MonadIO m => Bolt.Pipe -> ConduitT EC2.Vpc o m ()
ingestVpcs db = mapM_C ingestVpc
 where
  ingestVpc :: MonadIO m => EC2.Vpc -> m ()
  ingestVpc vpc =
    Bolt.run db $ do
      Bolt.queryP_ "CREATE (v:Vpc $v)" (Bolt.props ["v" =: vpc])
      traverse_
        ( \tags ->
            Bolt.queryP_
              "MATCH (v:Vpc) WHERE v.vpcId = $v CREATE (v)-[:HAS_TAGS]->(t:Tags $t)"
              (Bolt.props ["v" =: Vpc.vpcId vpc, "t" =: Tags tags])
        )
        (Vpc.tags vpc)
      (traverse_ . traverse_)
        ( \cidr ->
            Bolt.queryP_
              "MATCH (v:Vpc) WHERE v.vpcId = $v CREATE (v)-[:ASSOC_CIDR {associationId: $c.associationId}]->(c:Cidr $c)"
              (Bolt.props ["v" =: Vpc.vpcId vpc, "c" =: cidr])
        )
        (Vpc.cidrBlockAssociationSet vpc)
      (traverse_ . traverse_)
        ( \cidr ->
            Bolt.queryP_
              "MATCH (v:Vpc) WHERE v.vpcId = $v CREATE (v)-[:ASSOC_CIDR {associationId: $c.associationId}]->(c:Ipv6Cidr $c)"
              (Bolt.props ["v" =: Vpc.vpcId vpc, "c" =: cidr])
        )
        (Vpc.ipv6CidrBlockAssociationSet vpc)

fetchAllSubnets :: MonadResource m => AWS.Env -> ConduitM () EC2.Subnet m ()
fetchAllSubnets env = AWS.paginate env EC2.newDescribeSubnets .| concatMapC extractSubnets

extractSubnets :: EC2.DescribeSubnetsResponse -> [EC2.Subnet]
extractSubnets = concat . EC2.subnets

instance Bolt.IsValue EC2.Subnet where
  toValue Subnet.Subnet'{..} =
    Bolt.toValue $
      Map.fromList
        [ "ownerId" =: ownerId
        , "subnetId" =: subnetId
        , "subnetArn" =: subnetArn
        , "cidrBlock" =: cidrBlock
        , "availabilityZoneId" =: availabilityZoneId
        , "availabilityZone" =: availabilityZone
        , "vpcId" =: vpcId
        ]

ingestSubnets :: MonadIO m => Bolt.Pipe -> ConduitT EC2.Subnet o m ()
ingestSubnets db = mapM_C ingestSubnet
 where
  ingestSubnet :: MonadIO m => EC2.Subnet -> m ()
  ingestSubnet subnet = do
    Bolt.run db $ do
      Bolt.queryP_ "CREATE (s:Subnet $s)" (Bolt.props ["s" =: subnet])
      traverse_
        ( \tags ->
            Bolt.queryP_
              "MATCH (s:Subnet) WHERE s.subnetId = $s CREATE (s)-[:HAS_TAGS]->(t:Tags $t)"
              (Bolt.props ["s" =: Subnet.subnetId subnet, "t" =: Tags tags])
        )
        (Subnet.tags subnet)

fetchAllLambdas :: MonadResource m => AWS.Env -> ConduitM () Lambda.FunctionConfiguration m ()
fetchAllLambdas env = AWS.paginate env Lambda.newListFunctions .| concatMapC extractLambdas

extractLambdas :: Lambda.ListFunctionsResponse -> [Lambda.FunctionConfiguration]
extractLambdas = concat . Lambda.functions

instance Bolt.IsValue Lambda.FunctionConfiguration where
  toValue FuncCfg.FunctionConfiguration'{..} =
    Bolt.toValue $
      Map.fromList
        [ "functionName" =: functionName
        , "functionArn" =: functionArn
        , "timeout" =: timeout
        , "memorySize" =: memorySize
        , "handler" =: handler
        , "role" =: role'
        , "version" =: version
        , "codeSize" =: codeSize
        , "vpcId" =: (VpcCfg.vpcId <$> vpcConfig)
        , "securityGroupsIds" =: (VpcCfg.securityGroupIds <$> vpcConfig)
        , "subnetIds" =: (VpcCfg.subnetIds <$> vpcConfig)
        ]

ingestLambdas :: MonadIO m => Bolt.Pipe -> ConduitT Lambda.FunctionConfiguration o m ()
ingestLambdas db = mapM_C ingestLambda
 where
  ingestLambda :: MonadIO m => Lambda.FunctionConfiguration -> m ()
  ingestLambda lambda = do
    Bolt.run db $ do
      Bolt.queryP_ "CREATE (l:Lambda $l)" (Bolt.props ["l" =: lambda])