{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

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
import qualified Amazonka.Lambda.Types.FunctionConfiguration as FunctionConfiguration
import qualified Amazonka.RDS as RDS
import qualified Amazonka.RDS.DescribeDBInstances as DescribeDbInstances
import qualified Amazonka.RDS.Types.DBInstance as DBInstance
import qualified Amazonka.ResourceGroupsTagging as ResourceGroupsTagging
import qualified Amazonka.ResourceGroupsTagging.GetResources as GetResources
import qualified Amazonka.ResourceGroupsTagging.Types.ResourceTagMapping as ResourceTagMapping
import Conduit
import Control.Concurrent.Async (mapConcurrently_)
import Control.Monad.Trans.Resource
import Data.Default
import Data.Foldable (traverse_)
import qualified Data.HashMap.Strict as HashMap
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

newtype Tags = Tags (Map.Map Text Bolt.Value)

class IsTag a where
  toTags :: a -> Tags

instance IsTag [EC2.Tag] where
  toTags = Tags . Map.fromList . map tagPair
   where
    tagPair (EC2.Tag' k v) = k =: Just v

instance IsTag [RDS.Tag] where
  toTags = Tags . Map.fromList . mapMaybe tagPair
   where
    tagPair (RDS.Tag' (Just k) v) = Just $ k =: Just v
    tagPair (RDS.Tag' Nothing _) = Nothing

instance IsTag [ResourceGroupsTagging.Tag] where
  toTags = Tags . Map.fromList . map tagPair
   where
    tagPair (ResourceGroupsTagging.Tag' k v) = k =: Just v

instance IsTag (HashMap.HashMap Text Text) where
  toTags = Tags . Map.fromList . HashMap.toList . fmap Bolt.toValue

instance Bolt.IsValue Tags where
  toValue (Tags tags) = Bolt.toValue tags

discover :: Amazonka.Env -> Bolt.BoltCfg -> IO ()
discover env boltcfg =
  do
    runResourceT $ withBolt boltcfg $ \db -> Bolt.run db $ do
      Bolt.query_ "MATCH (r:Resource) DETACH DELETE r"
      Bolt.query_ "MATCH (i:Instance) DETACH DELETE i"
      Bolt.query_ "MATCH (v:Vpc) DETACH DELETE v"
      Bolt.query_ "MATCH (c:Cidr) DETACH DELETE c"
      Bolt.query_ "MATCH (c:Ipv6Cidr) DETACH DELETE c"
      Bolt.query_ "MATCH (s:Subnet) DETACH DELETE s"
      Bolt.query_ "MATCH (t:Tags) DETACH DELETE t"
      Bolt.query_ "MATCH (l:Lambda) DETACH DELETE l"
      Bolt.query_ "MATCH (g:SecurityGroup) DETACH DELETE g"
      Bolt.query_ "MATCH (p:IpPermission) DETACH DELETE p"
      Bolt.query_ "MATCH (i:DbInstance) DETACH DELETE i"
      Bolt.query_ "MATCH (e:EndPoint) DETACH DELETE e"
      Bolt.query_ "MATCH (e:Environment) DETACH DELETE e"
    mapConcurrently_
      run
      [ \db -> fetchAllResourceTagMappings env .| ingestResourceTags db
      , \db -> fetchAllEc2Instances env .| ingestInstances db
      , \db -> fetchAllSubnets env .| ingestSubnets db
      , \db -> fetchAllVpcs env .| ingestVpcs db
      , \db -> fetchAllLambdas env .| ingestLambdas db
      , \db -> fetchAllSecurityGroups env .| ingestSecurityGroups db
      , \db -> fetchAllDbInstances env .| ingestDbInstances db
      ]
    runResourceT $ withBolt boltcfg $ \db -> Bolt.run db $ do
      Bolt.query_ "MATCH (i:Instance) MATCH (v:Vpc) WHERE i.vpcId = v.vpcId CREATE (i)-[:IN_VPC]->(v)"
      Bolt.query_ "MATCH (l:Lambda) MATCH (v:Vpc) WHERE l.vpcId = v.vpcId CREATE (l)-[:IN_VPC]->(v)"
      Bolt.query_ "MATCH (s:Subnet) MATCH (v:Vpc) WHERE s.vpcId = v.vpcId CREATE (v)-[:HAS_SUBNET]->(s)"
      Bolt.query_ "MATCH (l:Lambda) MATCH (s:Subnet) WHERE s.subnetId IN l.subnetIds MERGE (l)-[:HAS_SUBNET]->(s)"
      Bolt.query_ "MATCH (i:Instance) MATCH (g:SecurityGroup) WHERE g.groupId IN i.securityGroups MERGE (i)-[:HAS_SECURITY_GROUP]->(g)"
      Bolt.query_ "MATCH (l:Lambda) MATCH (g:SecurityGroup) WHERE g.groupId IN l.securityGroupsIds MERGE (l)-[:HAS_SECURITY_GROUP]->(g)"
      Bolt.query_ "MATCH (d:DbInstance) MATCH (g:SecurityGroup) WHERE g.groupId IN d.vpcSecurityGroups MERGE (d)-[:HAS_SECURITY_GROUP]->(g)"
      Bolt.query_ "MATCH (d:DbInstance) MATCH (g:SecurityGroup) WHERE g.groupId IN d.dbSecurityGroups MERGE (d)-[:HAS_SECURITY_GROUP]->(g)"
      Bolt.query_ "MATCH (e:Environment) WHERE (ANY(prop IN KEYS(e) WHERE TOSTRING(e[prop]) ENDS WITH \"rds.amazonaws.com\")) WITH e, [prop IN KEYS(e) WHERE TOSTRING(e[prop]) ENDS WITH \"rds.amazonaws.com\"| e[prop]] AS rds MATCH (ep:Endpoint) WHERE ep.address IN rds MERGE (e)-[:REFERENCES]->(ep)"
      Bolt.query_ "MATCH (a)-[:HAS_ENVIRONMENT]->(e)-[:REFERENCES]->(ep:Endpoint)<-[:ENDPOINT]-(d:DbInstance) MERGE (a)-[:USES_DATABASE]->(d)"
      Bolt.query_ "MATCH (t:Tags) WHERE ANY(p IN KEYS(t) WHERE p = 'app') MERGE (a:App {name: t.app}) MERGE (t)-[:REFERENCES]->(a)"
      Bolt.query_ "MATCH (t:Tags) WHERE ANY(p IN KEYS(t) WHERE p = 'service') MERGE (a:Service {name: t.service}) MERGE (t)-[:REFERENCES]->(a)"
      Bolt.query_ "MATCH (t:Tags) WHERE ANY(p IN KEYS(t) WHERE p = 'system') MERGE (a:System {name: t.system}) MERGE (t)-[:REFERENCES]->(a)"
      Bolt.query_ "MATCH (t:Tags) WHERE ANY(p IN KEYS(t) WHERE p = 'team') MERGE (a:Team {name: t.team}) MERGE (t)-[:REFERENCES]->(a)"
      Bolt.query_ "MATCH (a)-[:HAS_TAGS]->(e)-[:REFERENCES]->(b:App) MERGE (a)-[:APP_COMPONENT]->(b)"
      Bolt.query_ "MATCH (a)-[:HAS_TAGS]->(e)-[:REFERENCES]->(b:Service) MERGE (a)-[:SERVICE_COMPONENT]->(b)"
      Bolt.query_ "MATCH (a)-[:HAS_TAGS]->(e)-[:REFERENCES]->(b:System) MERGE (a)-[:SYSTEM_COMPONENT]->(b)"
      Bolt.query_ "MATCH (a)-[:HAS_TAGS]->(e)-[:REFERENCES]->(b:Team) MERGE (b)-[:OWNS]->(a)"
      Bolt.query_ "MATCH (s)<-[:SERVICE_COMPONENT]-(x)-[:APP_COMPONENT]->(a) MERGE (s)-[:SERVICE_OF]->(a)"
      Bolt.query_ "MATCH (s)<-[:SYSTEM_COMPONENT]-(x)-[:APP_COMPONENT]->(a) MERGE (a)-[:APP_OF]->(s)"
 where
  run :: MonadUnliftIO m => (Bolt.Pipe -> ConduitT () Void (ResourceT m) a) -> m a
  run c = runResourceT $ withBolt boltcfg (runConduit . c)

fetchAllResourceTagMappings :: MonadResource m => Amazonka.Env -> ConduitM () ResourceGroupsTagging.ResourceTagMapping m ()
fetchAllResourceTagMappings env =
  Amazonka.paginate env ResourceGroupsTagging.newGetResources
    .| concatMapC (concat . GetResources.resourceTagMappingList)

ingestResourceTags :: MonadIO m => Bolt.Pipe -> ConduitT ResourceGroupsTagging.ResourceTagMapping o m ()
ingestResourceTags db = mapM_C ingestTags
 where
  ingestTags :: MonadIO m => ResourceGroupsTagging.ResourceTagMapping -> m ()
  ingestTags mapping =
    Bolt.run db $ do
      traverse_
        ( \arn -> do
            Bolt.queryP_ "MERGE (r:Resource {resourceARN: $r})" (Bolt.props ["r" =: arn])
            traverse_
              ( \tags ->
                  Bolt.queryP_
                    "MATCH (r:Resource) WHERE r.resourceARN = $r CREATE (t:Tags $t) MERGE (r)-[:HAS_TAGS]->(t)"
                    (Bolt.props ["r" =: arn, "t" =: toTags tags])
              )
              (ResourceTagMapping.tags mapping)
        )
        (ResourceTagMapping.resourceARN mapping)

fetchAllEc2Instances :: MonadResource m => Amazonka.Env -> ConduitM () (Amazonka.Region, Text, Instance.Instance) m ()
fetchAllEc2Instances env =
  Amazonka.paginate env DescribeInstances.newDescribeInstances
    .| reservations
    .| instances
 where
  reservations :: MonadResource m => ConduitM DescribeInstances.DescribeInstancesResponse Reservation.Reservation m ()
  reservations =
    concatMapC (concat . DescribeInstances.reservations)

  instances :: MonadResource m => ConduitM Reservation.Reservation (Amazonka.Region, Text, Instance.Instance) m ()
  instances =
    concatMapC $ \r ->
      maybe [] (map (Amazonka.envRegion env,Reservation.ownerId r,)) (Reservation.instances r)

ingestInstances :: MonadIO m => Bolt.Pipe -> ConduitT (Amazonka.Region, Text, Instance.Instance) o m ()
ingestInstances db = mapM_C ingestInstance
 where
  ingestInstance :: MonadIO m => (Amazonka.Region, Text, Instance.Instance) -> m ()
  ingestInstance (region, owner, inst) =
    Bolt.run db $ do
      Bolt.queryP_
        "MERGE (r:Resource {resourceARN:$r}) ON CREATE SET r:Instance, r += $i ON MATCH SET r:Instance, r += $i"
        (Bolt.props ["i" =: inst, "r" =: arn])
   where
    arn = "arn:aws:ec2:" <> Amazonka.fromRegion region <> ":" <> owner <> ":instance/" <> Instance.instanceId inst

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
              (Bolt.props ["v" =: Vpc.vpcId vpc, "t" =: toTags tags])
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
              (Bolt.props ["s" =: Subnet.subnetId subnet, "t" =: toTags tags])
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
      Bolt.queryP_
        "MERGE (r:Resource {resourceARN:$l.functionArn}) ON CREATE SET r:Lambda, r += $l ON MATCH SET r:Lambda, r += $l"
        (Bolt.props ["l" =: lambda])
      traverse_
        ( \env ->
            Bolt.queryP_
              "MATCH (l:Lambda) WHERE l.functionArn = $l CREATE (l)-[:HAS_ENVIRONMENT]->(e:Environment $e)"
              (Bolt.props ["l" =: FunctionConfiguration.functionArn lambda, "e" =: env])
        )
        (FunctionConfiguration.environment lambda)

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
              (Bolt.props ["g" =: SecurityGroup.groupId group, "t" =: toTags tags])
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

fetchAllDbInstances :: MonadResource m => Amazonka.Env -> ConduitM () DBInstance.DBInstance m ()
fetchAllDbInstances env =
  Amazonka.paginate env DescribeDbInstances.newDescribeDBInstances
    .| concatMapC (concat . DescribeDbInstances.dbInstances)

ingestDbInstances :: MonadIO m => Bolt.Pipe -> ConduitT DBInstance.DBInstance o m ()
ingestDbInstances db = mapM_C ingestDbInstance
 where
  ingestDbInstance :: MonadIO m => DBInstance.DBInstance -> m ()
  ingestDbInstance inst = do
    Bolt.run db $ do
      Bolt.queryP_ "CREATE (d:DbInstance $d)" (Bolt.props ["d" =: inst])
      traverse_
        ( \tags ->
            Bolt.queryP_
              "MATCH (d:DbInstance) WHERE d.dbInstanceArn = $d CREATE (d)-[:HAS_TAGS]->(t:Tags $t)"
              (Bolt.props ["d" =: DBInstance.dbInstanceArn inst, "t" =: toTags tags])
        )
        (DBInstance.tagList inst)
      traverse_
        ( \e ->
            Bolt.queryP_
              "MATCH (d:DbInstance) WHERE d.dbInstanceArn = $d MERGE (e:Endpoint {address: $e.address, port: $e.port, hostedZoneId: $e.hostedZoneId}) MERGE (d)-[:ENDPOINT]->(e)"
              (Bolt.props ["d" =: DBInstance.dbInstanceArn inst, "e" =: e])
        )
        (DBInstance.endpoint inst)
      traverse_
        ( \e ->
            Bolt.queryP_
              "MATCH (d:DbInstance) WHERE d.dbInstanceArn = $d MERGE (e:Endpoint {address: $e.address, port: $e.port, hostedZoneId: $e.hostedZoneId}) MERGE (d)-[:LISTENER_ENDPOINT]->(e)"
              (Bolt.props ["d" =: DBInstance.dbInstanceArn inst, "e" =: e])
        )
        (DBInstance.listenerEndpoint inst)

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