{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Amazonka
import qualified Amazonka.EC2 as EC2
import qualified Amazonka.EC2.DescribeInstances as DescribeInstances
import qualified Amazonka.EC2.DescribeSecurityGroups as DescribeSecurityGroups
import qualified Amazonka.EC2.DescribeSubnets as DescribeSubnets
import qualified Amazonka.EC2.DescribeVpcs as DescribeVpcs
import qualified Amazonka.EC2.Types.Instance as Instance
import qualified Amazonka.EC2.Types.Reservation as Reservation
import qualified Amazonka.EC2.Types.SecurityGroup as SecurityGroup
import qualified Amazonka.EC2.Types.Subnet as Subnet
import qualified Amazonka.EC2.Types.Vpc as Vpc
import qualified Amazonka.Lambda as Lambda
import qualified Amazonka.Lambda.ListFunctions as ListFunctions
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
import Orphans ()
import System.IO (stdout)

data Config = Config
  { host :: String
  , user :: Text
  , password :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Yaml.FromJSON)

newtype Tags = Tags [EC2.Tag]

instance Bolt.IsValue Tags where
  toValue (Tags tags) = Bolt.toValue $ tagMap tags
   where
    tagPair (EC2.Tag' k v) = k =: v
    tagMap = Map.fromList . map tagPair

discover :: Amazonka.Env -> Bolt.BoltCfg -> IO ()
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
      Bolt.query_ "MATCH (g:SecurityGroup) DETACH DELETE g"
      Bolt.query_ "MATCH (p:IpPermission) DETACH DELETE p"
    mapConcurrently_
      run
      [ \db -> fetchAllEc2Instances env .| ingestInstances db
      , \db -> fetchAllSubnets env .| ingestSubnets db
      , \db -> fetchAllVpcs env .| ingestVpcs db
      , \db -> fetchAllLambdas env .| ingestLambdas db
      , \db -> fetchAllSecurityGroups env .| ingestSecurityGroups db
      ]
    runResourceT $ withBolt boltcfg $ \db -> Bolt.run db $ do
      Bolt.query_ "MATCH (i:Instance), (v:Vpc) WHERE i.vpcId = v.vpcId CREATE (i)-[:IN_VPC]->(v)"
      Bolt.query_ "MATCH (l:Lambda), (v:Vpc) WHERE l.vpcId = v.vpcId CREATE (l)-[:IN_VPC]->(v)"
      Bolt.query_ "MATCH (s:Subnet), (v:Vpc) WHERE s.vpcId = v.vpcId CREATE (v)-[:HAS_SUBNET]->(s)"
      Bolt.query_ "MATCH (l:Lambda), (s:Subnet) WHERE s.subnetId IN l.subnetIds MERGE (l)-[:HAS_SUBNET]->(s)"
      Bolt.query_ "MATCH (i:Instance), (g:SecurityGroup) WHERE g.groupId IN i.securityGroups MERGE (l)-[:HAS_SECURITY_GROUP]->(g)"
      Bolt.query_ "MATCH (l:Lambda), (g:SecurityGroup) WHERE g.groupId IN l.securityGroupsIds MERGE (l)-[:HAS_SECURITY_GROUP]->(g)"
 where
  run :: MonadUnliftIO m => (Bolt.Pipe -> ConduitT () Void (ResourceT m) a) -> m a
  run c = runResourceT $ withBolt boltcfg (runConduit . c)

fetchAllEc2Instances :: MonadResource m => Amazonka.Env -> ConduitM () Instance.Instance m ()
fetchAllEc2Instances env =
  Amazonka.paginate env DescribeInstances.newDescribeInstances
    .| concatMapC (concat . mapMaybe Reservation.instances . concat . DescribeInstances.reservations)

ingestInstances :: MonadIO m => Bolt.Pipe -> ConduitT Instance.Instance o m ()
ingestInstances db = mapM_C ingestInstance
 where
  ingestInstance :: MonadIO m => Instance.Instance -> m ()
  ingestInstance inst =
    Bolt.run db $ do
      Bolt.queryP_ "CREATE (i:Instance $i)" (Bolt.props ["i" =: inst])
      traverse_
        ( \tags ->
            Bolt.queryP_
              "MATCH (i:Instance) WHERE i.instanceId = $i CREATE (i)-[:HAS_TAGS]->(t:Tags $t)"
              (Bolt.props ["i" =: Instance.instanceId inst, "t" =: Tags tags])
        )
        (Instance.tags inst)

fetchAllVpcs :: MonadResource m => Amazonka.Env -> ConduitM () Vpc.Vpc m ()
fetchAllVpcs env =
  Amazonka.paginate env DescribeVpcs.newDescribeVpcs
    .| concatMapC (concat . DescribeVpcs.vpcs)

ingestVpcs :: MonadIO m => Bolt.Pipe -> ConduitT Vpc.Vpc o m ()
ingestVpcs db = mapM_C ingestVpc
 where
  ingestVpc :: MonadIO m => Vpc.Vpc -> m ()
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

fetchAllSubnets :: MonadResource m => Amazonka.Env -> ConduitM () Subnet.Subnet m ()
fetchAllSubnets env =
  Amazonka.paginate env DescribeSubnets.newDescribeSubnets
    .| concatMapC (concat . DescribeSubnets.subnets)

ingestSubnets :: MonadIO m => Bolt.Pipe -> ConduitT Subnet.Subnet o m ()
ingestSubnets db = mapM_C ingestSubnet
 where
  ingestSubnet :: MonadIO m => Subnet.Subnet -> m ()
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

fetchAllLambdas :: MonadResource m => Amazonka.Env -> ConduitM () Lambda.FunctionConfiguration m ()
fetchAllLambdas env =
  Amazonka.paginate env Lambda.newListFunctions
    .| concatMapC (concat . ListFunctions.functions)

ingestLambdas :: MonadIO m => Bolt.Pipe -> ConduitT Lambda.FunctionConfiguration o m ()
ingestLambdas db = mapM_C ingestLambda
 where
  ingestLambda :: MonadIO m => Lambda.FunctionConfiguration -> m ()
  ingestLambda lambda = do
    Bolt.run db $ do
      Bolt.queryP_ "CREATE (l:Lambda $l)" (Bolt.props ["l" =: lambda])

fetchAllSecurityGroups :: MonadResource m => Amazonka.Env -> ConduitM () SecurityGroup.SecurityGroup m ()
fetchAllSecurityGroups env =
  Amazonka.paginate env DescribeSecurityGroups.newDescribeSecurityGroups
    .| concatMapC (concat . DescribeSecurityGroups.securityGroups)

ingestSecurityGroups :: MonadIO m => Bolt.Pipe -> ConduitT SecurityGroup.SecurityGroup o m ()
ingestSecurityGroups db = mapM_C ingestSecurityGroup
 where
  ingestSecurityGroup :: MonadIO m => SecurityGroup.SecurityGroup -> m ()
  ingestSecurityGroup group = do
    Bolt.run db $ do
      Bolt.queryP_ "CREATE (g:SecurityGroup $g)" (Bolt.props ["g" =: group])
      traverse_
        ( \tags ->
            Bolt.queryP_
              "MATCH (g:SecurityGroup) WHERE g.groupId = $g CREATE (g)-[:HAS_TAGS]->(t:Tags $t)"
              (Bolt.props ["g" =: SecurityGroup.groupId group, "t" =: Tags tags])
        )
        (SecurityGroup.tags group)
      (traverse_ . traverse_)
        ( \ipPerm ->
            Bolt.queryP_
              "MATCH (g:SecurityGroup) WHERE g.groupId = $g CREATE (g)-[:INGRESS]->(p:IpPermission $p)"
              (Bolt.props ["g" =: SecurityGroup.groupId group, "p" =: ipPerm])
        )
        (SecurityGroup.ipPermissions group)
      (traverse_ . traverse_)
        ( \ipPerm -> do
            Bolt.queryP_
              "MATCH (g:SecurityGroup) WHERE g.groupId = $g CREATE (g)-[:EGRESS]->(p:IpPermission $p)"
              (Bolt.props ["g" =: SecurityGroup.groupId group, "p" =: ipPerm])
        )
        (SecurityGroup.ipPermissionsEgress group)

withBolt :: MonadResource m => Bolt.BoltCfg -> (Bolt.Pipe -> m a) -> m a
withBolt cfg f = do
  (key, db) <- allocate (Bolt.connect cfg) Bolt.close
  r <- f db
  release key
  return r

main :: IO ()
main = do
  cfg <- Yaml.decodeFileThrow "aws-discover.yaml"
  lgr <- Amazonka.newLogger Amazonka.Info stdout
  discoveredEnv <- Amazonka.newEnv Amazonka.discover
  let env =
        discoveredEnv
          { Amazonka.envLogger = lgr
          , Amazonka.envRegion = Amazonka.Ireland
          }
      boltcfg =
        def
          { Bolt.host = host cfg
          , Bolt.user = user cfg
          , Bolt.password = password cfg
          }
  discover env boltcfg