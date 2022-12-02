{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

module Main where

-- import qualified Amazonka
-- import qualified Amazonka.APIGateway as APIGateway
-- import qualified Amazonka.APIGateway.GetBasePathMappings as GetBasePathMappings
-- import qualified Amazonka.APIGateway.GetDomainNames as GetDomainNames
-- import qualified Amazonka.APIGateway.GetResources as ApiGetResources
-- import qualified Amazonka.APIGateway.GetRestApis as GetRestApis
-- import qualified Amazonka.APIGateway.Types.BasePathMapping as BasePathMapping
-- import qualified Amazonka.APIGateway.Types.DomainName as DomainName
-- import qualified Amazonka.APIGateway.Types.Method as Method
-- import qualified Amazonka.APIGateway.Types.Resource as Resource
-- import qualified Amazonka.APIGateway.Types.RestApi as RestApi
-- import qualified Amazonka.CloudWatchLogs.DescribeLogGroups as DescribeLogGroups
-- import qualified Amazonka.CloudWatchLogs.Types.LogGroup as LogGroup
-- import qualified Amazonka.Lambda as Lambda
-- import qualified Amazonka.Lambda.ListFunctions as ListFunctions
-- import qualified Amazonka.Lambda.Types.FunctionConfiguration as FunctionConfiguration
-- import qualified Amazonka.RDS.DescribeDBInstances as DescribeDbInstances
-- import qualified Amazonka.RDS.Types.DBInstance as DBInstance
-- import qualified Amazonka.ResourceGroupsTagging as ResourceGroupsTagging
-- import qualified Amazonka.ResourceGroupsTagging.GetResources as GetResources
-- import qualified Amazonka.ResourceGroupsTagging.Types.ResourceTagMapping as ResourceTagMapping
-- import qualified Amazonka.SecretsManager as SecretsManager
-- import qualified Amazonka.SecretsManager.GetSecretValue as GetSecretValue
-- import Conduit
-- import Config

-- import Control.Applicative ((<|>))
-- import Control.Concurrent.Async (mapConcurrently_)
-- import qualified Control.Monad.Catch as Catch
-- import qualified Data.Aeson as Aeson
-- import Data.Foldable (traverse_)
-- import qualified Data.HashMap.Strict as HashMap
-- import qualified Data.Map.Strict as Map
-- import Data.Maybe (isJust)
-- import Data.Text (Text)
-- import qualified Data.Text as Text
-- import Data.Text.Encoding as Text
-- import Data.Traversable (for)
-- import Database

-- import Database.Bolt ((=:))
-- import qualified Database.Bolt as Bolt
-- import Orphans ()
-- import System.IO (stdout)
-- import Tags

-- import Hasql.Decoders
-- import Hasql.Encoders
-- import Database.Bolt ((=:))
-- import qualified Database.Bolt as Bolt
-- import Orphans ()
-- import System.IO (stdout)
-- import Tags

-- import Hasql.Decoders
-- import Hasql.Encoders
-- import Hasql.Session (run)

-- import Data.Aeson
-- import Data.Text (Text)
-- import qualified Data.UUID.V4 as V4
-- import Database.Types
-- import GEXF (writeGEXF)

import Data.Function ((&))
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.UUID as UUID
import Graph

{-
discover :: Amazonka.Env -> Bolt.BoltCfg -> IO ()
discover env boltcfg = do
  cleanup
  mapConcurrently_
    run
    [ \db -> fetchAllResourceTagMappings env .| ingestResourceTags db
    , \db -> fetchAllEc2Instances env .| ingestInstances db
    , \db -> fetchAllSubnets env .| ingestSubnets db
    , \db -> fetchAllVpcs env .| ingestVpcs db
    , \db -> fetchAllLambdas env .| ingestLambdas db
    , \db -> fetchAllSecurityGroups env .| ingestSecurityGroups db
    , \db -> fetchAllDbInstances env .| ingestDbInstances db
    , \db -> fetchAllRestApis env .| fetchAllRestApiResources env .| fetchAllRestApiMethods env .| ingestRestApis db
    , \db -> fetchAllDomainNames env .| fetchAllBasePathMappings env .| ingestDomainNames db
    , \db -> fetchAllLogGroups env .| ingestLogGroups db
    ]
  run $ \db -> findEnvReferredSecrets db .| fetchSecrets env .| ingestSecrets db
  relate
 where
  run :: MonadUnliftIO m => (Bolt.Pipe -> ConduitT () Void (ResourceT m) a) -> m a
  run c = runResourceT $ withBolt boltcfg (runConduit . c)

  cleanup :: IO ()
  cleanup = do
    runResourceT $ withBolt boltcfg $ \db -> Bolt.run db $ do
      Bolt.query_ "MATCH (c:Cidr) DETACH DELETE c"
      Bolt.query_ "MATCH (c:Ipv6Cidr) DETACH DELETE c"
      Bolt.query_ "MATCH (e:EndPoint) DETACH DELETE e"
      Bolt.query_ "MATCH (e:Environment) DETACH DELETE e"
      Bolt.query_ "MATCH (g:SecurityGroup) DETACH DELETE g"
      Bolt.query_ "MATCH (i:DbInstance) DETACH DELETE i"
      Bolt.query_ "MATCH (i:Instance) DETACH DELETE i"
      Bolt.query_ "MATCH (l:Lambda) DETACH DELETE l"
      Bolt.query_ "MATCH (m:SecretsMap) DETACH DELETE m"
      Bolt.query_ "MATCH (p:IpPermission) DETACH DELETE p"
      Bolt.query_ "MATCH (s:Secrets) DETACH DELETE s"
      Bolt.query_ "MATCH (s:Subnet) DETACH DELETE s"
      Bolt.query_ "MATCH (t:Tags) DETACH DELETE t"
      Bolt.query_ "MATCH (v:Vpc) DETACH DELETE v"
      Bolt.query_ "MATCH (a:RestApi) DETACH DELETE a"
      Bolt.query_ "MATCH (r:RestResource) DETACH DELETE r"
      Bolt.query_ "MATCH (m:RestMethod) DETACH DELETE m"
      Bolt.query_ "MATCH (i:MethodIntegration) DETACH DELETE i"
      Bolt.query_ "MATCH (d:DomainName) DETACH DELETE d"
      Bolt.query_ "MATCH (m:BasePathMapping) DETACH DELETE m"
      Bolt.query_ "MATCH (g:LogGroup) DETACH DELETE g"
      Bolt.query_ "MATCH (r:Resource) DETACH DELETE r"

  relate :: IO ()
  relate = do
    runResourceT $ withBolt boltcfg $ \db -> Bolt.run db $ do
      Bolt.query_ "MATCH (i:Instance) MATCH (v:Vpc) WHERE i.vpcId = v.vpcId CREATE (i)-[:IN_VPC]->(v)"
      Bolt.query_ "MATCH (l:Lambda) MATCH (v:Vpc) WHERE l.vpcId = v.vpcId CREATE (l)-[:IN_VPC]->(v)"
      Bolt.query_ "MATCH (s:Subnet) MATCH (v:Vpc) WHERE s.vpcId = v.vpcId CREATE (v)-[:HAS_SUBNET]->(s)"
      Bolt.query_ "MATCH (l:Lambda) MATCH (s:Subnet) WHERE s.subnetId IN l.subnetIds MERGE (l)-[:HAS_SUBNET]->(s)"
      Bolt.query_ "MATCH (i:Instance) MATCH (g:SecurityGroup) WHERE g.groupId IN i.securityGroups MERGE (i)-[:HAS_SECURITY_GROUP]->(g)"
      Bolt.query_ "MATCH (l:Lambda) MATCH (g:SecurityGroup) WHERE g.groupId IN l.securityGroupsIds MERGE (l)-[:HAS_SECURITY_GROUP]->(g)"
      Bolt.query_ "MATCH (d:DbInstance) MATCH (g:SecurityGroup) WHERE g.groupId IN d.vpcSecurityGroups MERGE (d)-[:HAS_SECURITY_GROUP]->(g)"
      Bolt.query_ "MATCH (d:DbInstance) MATCH (g:SecurityGroup) WHERE g.groupId IN d.dbSecurityGroups MERGE (d)-[:HAS_SECURITY_GROUP]->(g)"
      Bolt.query_ "MATCH (dn:DomainName)--(m:BasePathMapping) MATCH (r:RestApi) WHERE r.id = m.restApiId MERGE (m)-[:MAPS_TO]->(r) MERGE (dn)-[:HAS_API_MAPPING {path: m.basePath, stage: m.stage}]->(r)"
      Bolt.query_ "MATCH (e:Environment) WHERE ANY(p IN KEYS(e) WHERE TOSTRING(e[p]) ENDS WITH \"rds.amazonaws.com\") WITH e, [p IN KEYS(e) WHERE TOSTRING(e[p]) ENDS WITH \"rds.amazonaws.com\"| e[p]] AS rds MATCH (ep:Endpoint) WHERE ep.address IN rds MERGE (e)-[:REFERENCES]->(ep)"
      Bolt.query_ "MATCH (e:Environment) WHERE ANY(p IN KEYS(e) WHERE TOSTRING(e[p]) STARTS WITH \"arn:aws:secretsmanager\") UNWIND [p IN KEYS(e) WHERE TOSTRING(e[p]) STARTS WITH \"arn:aws:secretsmanager\" | e[p]] AS a MATCH (s:Secrets {arn:a}) MERGE (e)-[:REFERENCES]->(s)"
      Bolt.query_ "MATCH (dn:DomainName) MATCH (e:Environment) UNWIND([p IN keys(e) WHERE e[p] CONTAINS dn.domainName]) AS p MERGE (e)-[:REFERENCES {viaEnvironmentVariable: p}]->(dn)"
      Bolt.query_ "MATCH (a)-[:HAS_ENVIRONMENT]->(e)-[:REFERENCES]->(ep:Endpoint)<-[:ENDPOINT]-(d:DbInstance) MERGE (a)-[r:USES_DATABASE]->(d) SET r.inSecrets = true"
      Bolt.query_ "MATCH (a)-[:HAS_ENVIRONMENT]->(e)-[:REFERENCES]->(s:Secrets)-[:HAS_VALUES]->(m:SecretsMap) MERGE (a)-[:HAS_SECRETS]->(m)"
      Bolt.query_ "MATCH (a)-[:HAS_ENVIRONMENT]->(e)-[:REFERENCES]->(dn:DomainName)-[:HAS_API_MAPPING]->(r:RestApi) MERGE (a)-[:USES_API]->(r)"
      Bolt.query_ "MATCH (m:SecretsMap) WHERE (ANY(prop IN KEYS(m) WHERE TOSTRING(m[prop]) ENDS WITH \"rds.amazonaws.com\")) WITH m, [prop IN KEYS(m) WHERE TOSTRING(m[prop]) ENDS WITH \"rds.amazonaws.com\"| m[prop]] AS rds MATCH (ep:Endpoint) WHERE ep.address IN rds MERGE (m)-[:REFERENCES]->(ep)"
      Bolt.query_ "MATCH (a)-[:HAS_SECRETS]->(m)-[:REFERENCES]->(ep:Endpoint)<-[:ENDPOINT]-(d:DbInstance) MERGE (a)-[r:USES_DATABASE]->(d) SET r.inEnvironment = true"
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

{-
match (l:Lambda) match (a:MethodIntegration) where a.uri contains l.resourceARN merge (a)-[:HAS_INTEGRATION_ENDPOINT]->(l)
MATCH (a)-[:USES_API]->(b)-[:HAS_RESOURCE]->(c)-[:HAS_METHOD]->(d)-[:HAS_INTEGRATION]->(e)-[:HAS_INTEGRATION_ENDPOINT]->(f) merge (a)-[:CALLS {api: b.name, apiGateway: b.resourceARN, resource: c.path, method: d.httpMethod, integration: e.uri}]->(f)
-}

findEnvReferredSecrets :: MonadResource m => Bolt.Pipe -> ConduitM () Text m ()
findEnvReferredSecrets db =
  yieldMany
    =<< Bolt.run db (mapM (`Bolt.at` "arns") =<< Bolt.query "MATCH (e:Environment) WHERE ANY(p IN KEYS(e) WHERE e[p] STARTS WITH 'arn:aws:secretsmanager') UNWIND [p IN KEYS(e) WHERE e[p] STARTS WITH 'arn:aws:secretsmanager' | e[p]] AS arns RETURN DISTINCT arns")

fetchSecrets :: (Catch.MonadCatch m, MonadResource m, MonadIO m) => Amazonka.Env -> ConduitM Text GetSecretValue.GetSecretValueResponse m ()
fetchSecrets env = concatMapMC fetch
 where
  fetch arn =
    (Just <$> Amazonka.send env (SecretsManager.newGetSecretValue arn))
      `Catch.catch` (\(Amazonka.ServiceError se) -> liftIO (print se) >> return Nothing)

ingestSecrets :: MonadIO m => Bolt.Pipe -> ConduitT GetSecretValue.GetSecretValueResponse o m ()
ingestSecrets db = mapM_C ingestSecretValueResponse
 where
  ingestSecretValueResponse resp =
    Bolt.run db $ do
      Bolt.queryP_
        "MERGE (r:Resource {resourceARN:$s.arn}) ON CREATE SET r:Secrets, r += $s ON MATCH SET r:Secrets, r += $s"
        (Bolt.props ["s" =: resp])
      case secretMap resp of
        -- only store whitelisted fields if the secret is a map
        Just m ->
          Bolt.queryP_
            "MATCH (s:Secrets {arn: $s.arn}) CREATE (s)-[:HAS_VALUES]->(m:SecretsMap $m)"
            (Bolt.props ["s" =: resp, "m" =: m])
        -- don't store the secret if we don't know what type it is
        Nothing -> return ()

  secretMap :: GetSecretValue.GetSecretValueResponse -> Maybe (Map.Map Text Bolt.Value)
  secretMap resp = fmap Bolt.toValue . Map.filterWithKey safe <$> secret
   where
    secret =
      Aeson.decodeStrict
        =<< Text.encodeUtf8 . Amazonka.fromSensitive <$> GetSecretValue.secretString resp
          <|> Amazonka.unBase64 . Amazonka.fromSensitive <$> GetSecretValue.secretBinary resp

  safe :: Text -> Aeson.Value -> Bool
  safe key val = safeKey key || safeVal val

  safeKey key = any (`Text.isSuffixOf` key) ["_DB_ENGINE", "_DB_HOST", "_DB_NAME", "_DB_PORT"]
  safeVal (Aeson.String val) = "rds.amazonaws.com" `Text.isSuffixOf` val
  safeVal _ = False

fetchAllRestApis :: MonadResource m => Amazonka.Env -> ConduitM () APIGateway.RestApi m ()
fetchAllRestApis env =
  Amazonka.paginate env APIGateway.newGetRestApis
    .| concatMapC (concat . GetRestApis.items)

fetchAllRestApiResources :: MonadResource m => Amazonka.Env -> ConduitM APIGateway.RestApi (Amazonka.Region, APIGateway.RestApi, [APIGateway.Resource]) m ()
fetchAllRestApiResources env = mapMC fetchRestApiResources
 where
  fetchRestApiResources restApi = do
    res <-
      case RestApi.id restApi of
        Just restApiId ->
          runConduit $
            Amazonka.paginate env (ApiGetResources.newGetResources restApiId)
              .| concatMapC (concat . ApiGetResources.items)
              .| sinkList
        Nothing ->
          return []
    return (Amazonka.envRegion env, restApi, res)

fetchAllRestApiMethods :: MonadResource m => Amazonka.Env -> ConduitM (Amazonka.Region, APIGateway.RestApi, [APIGateway.Resource]) (Amazonka.Region, APIGateway.RestApi, [(APIGateway.Resource, [APIGateway.Method])]) m ()
fetchAllRestApiMethods env = mapMC fetchRestApiMethods
 where
  fetchRestApiMethods :: MonadResource m => (Amazonka.Region, APIGateway.RestApi, [APIGateway.Resource]) -> m (Amazonka.Region, APIGateway.RestApi, [(APIGateway.Resource, [APIGateway.Method])])
  fetchRestApiMethods (region, restApi, resources) =
    (region,restApi,) <$> methodsForResources (RestApi.id restApi) resources

  methodsForResources Nothing = return . map (,[])
  methodsForResources (Just restApiId) = traverse (methodsForResource restApiId)

  methodsForResource restApiId resource = do
    methods <- case Resource.id resource of
      Just resourceId ->
        for (maybe [] HashMap.keys $ Resource.resourceMethods resource) $ \method -> do
          Amazonka.send env (APIGateway.newGetMethod restApiId resourceId method)
      Nothing ->
        return []
    return (resource, methods)

ingestRestApis :: MonadIO m => Bolt.Pipe -> ConduitT (Amazonka.Region, APIGateway.RestApi, [(APIGateway.Resource, [APIGateway.Method])]) o m ()
ingestRestApis db = mapM_C ingestRestApi
 where
  ingestRestApi :: MonadIO m => (Amazonka.Region, APIGateway.RestApi, [(APIGateway.Resource, [APIGateway.Method])]) -> m ()
  ingestRestApi (region, restApi, resources) =
    Bolt.run db $ do
      case RestApi.id restApi of
        Just restApiId -> do
          let arn = "arn:aws:apigateway:" <> Amazonka.fromRegion region <> "::/restapis/" <> restApiId
          Bolt.queryP_
            "MERGE (r:Resource {resourceARN:$r}) ON CREATE SET r:RestApi, r += $a ON MATCH SET r:RestApi, r += $a"
            (Bolt.props ["r" =: arn, "a" =: restApi])
          traverse_
            ( \(resource, methods) -> do
                Bolt.queryP_
                  "MATCH (a:RestApi {id:$a}) MERGE (a)-[:HAS_RESOURCE]->(r:RestResource {id:$r.id}) ON CREATE SET r = $r ON MATCH SET r = $r"
                  (Bolt.props ["a" =: restApiId, "r" =: resource])
                traverse_
                  ( \method -> do
                      Bolt.queryP_
                        "MATCH (a:RestApi {id:$a})-[:HAS_RESOURCE]->(r:RestResource {id:$r}) MERGE (r)-[:HAS_METHOD]->(m:RestMethod {httpMethod:$m.httpMethod}) ON CREATE SET m = $m ON MATCH SET m = $m"
                        (Bolt.props ["a" =: restApiId, "r" =: Resource.id resource, "m" =: method])
                      traverse_
                        ( \integration ->
                            Bolt.queryP_
                              "MATCH (a:RestApi {id:$a})-[:HAS_RESOURCE]->(r:RestResource {id:$r})-[:HAS_METHOD]->(m:RestMethod {httpMethod:$m}) MERGE (m)-[:HAS_INTEGRATION]->(i:MethodIntegration) ON CREATE SET i = $i ON MATCH SET i = $i"
                              (Bolt.props ["a" =: restApiId, "r" =: Resource.id resource, "m" =: Method.httpMethod method, "i" =: integration])
                        )
                        (Method.methodIntegration method)
                  )
                  (filter (isJust . Method.httpMethod) methods)
            )
            resources
        Nothing ->
          return () -- Skip rest apis with no id

fetchAllDomainNames :: MonadResource m => Amazonka.Env -> ConduitM () (Amazonka.Region, APIGateway.DomainName) m ()
fetchAllDomainNames env =
  Amazonka.paginate env APIGateway.newGetDomainNames
    .| concatMapC (map (Amazonka.envRegion env,) . concat . GetDomainNames.items)

fetchAllBasePathMappings :: MonadResource m => Amazonka.Env -> ConduitM (Amazonka.Region, APIGateway.DomainName) (Amazonka.Region, APIGateway.DomainName, [APIGateway.BasePathMapping]) m ()
fetchAllBasePathMappings env = mapMC fetchBasePathMappings
 where
  fetchBasePathMappings (region, domainName) = do
    maps <-
      case DomainName.domainName domainName of
        Just name ->
          runConduit $
            Amazonka.paginate env (GetBasePathMappings.newGetBasePathMappings name)
              .| concatMapC (concat . GetBasePathMappings.items)
              .| sinkList
        Nothing ->
          return []
    return (region, domainName, maps)

ingestDomainNames :: MonadIO m => Bolt.Pipe -> ConduitT (Amazonka.Region, APIGateway.DomainName, [APIGateway.BasePathMapping]) o m ()
ingestDomainNames db = mapM_C ingestDomainName
 where
  ingestDomainName :: MonadIO m => (Amazonka.Region, APIGateway.DomainName, [APIGateway.BasePathMapping]) -> m ()
  ingestDomainName (region, domainName, mappings) =
    Bolt.run db $ do
      traverse_
        ( \name -> do
            let arn = "arn:aws:apigateway:" <> Amazonka.fromRegion region <> "::/domainnames/" <> name
            Bolt.queryP_
              "MERGE (r:Resource {resourceARN:$a}) ON CREATE SET r:DomainName, r += $r ON MATCH SET r:DomainName, r += $r"
              (Bolt.props ["a" =: arn, "r" =: domainName])
            traverse_
              ( \mapping -> do
                  Bolt.queryP_
                    "MATCH (r:DomainName {domainName: $n}) MERGE (r)-[:HAS_BASE_PATH_MAPPING]->(m:BasePathMapping {basePath: $m.basePath}) ON CREATE SET m = $m ON MATCH SET m = $m"
                    (Bolt.props ["n" =: name, "m" =: mapping])
              )
              (filter (isJust . BasePathMapping.basePath) mappings)
        )
        (DomainName.domainName domainName)

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

fetchAllVpcs :: MonadResource m => Amazonka.Env -> ConduitM () (Amazonka.Region, Vpc.Vpc) m ()
fetchAllVpcs env =
  Amazonka.paginate env DescribeVpcs.newDescribeVpcs
    .| concatMapC (map (Amazonka.envRegion env,) . concat . DescribeVpcs.vpcs)

ingestVpcs :: MonadIO m => Bolt.Pipe -> ConduitT (Amazonka.Region, Vpc.Vpc) o m ()
ingestVpcs db = mapM_C ingestVpc
 where
  ingestVpc :: MonadIO m => (Amazonka.Region, Vpc.Vpc) -> m ()
  ingestVpc (region, vpc) =
    Bolt.run db $ do
      case arn of
        Just r ->
          Bolt.queryP_
            "MERGE (r:Resource {resourceARN:$r}) ON CREATE SET r:Vpc, r += $v ON MATCH SET r:Vpc, r += $v"
            (Bolt.props ["v" =: vpc, "r" =: r])
        Nothing -> do
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
   where
    arn =
      case Vpc.ownerId vpc of
        Just owner -> Just $ "arn:aws:ec2:" <> Amazonka.fromRegion region <> ":" <> owner <> ":vpc/" <> Vpc.vpcId vpc
        Nothing -> Nothing

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
      Bolt.queryP_
        "MERGE (r:Resource {resourceARN:$s.subnetArn}) ON CREATE SET r:Subnet, r += $s ON MATCH SET r:Subnet, r += $s"
        (Bolt.props ["s" =: subnet])

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

fetchAllSecurityGroups :: MonadResource m => Amazonka.Env -> ConduitM () (Amazonka.Region, SecurityGroup.SecurityGroup) m ()
fetchAllSecurityGroups env =
  Amazonka.paginate env DescribeSecurityGroups.newDescribeSecurityGroups
    .| concatMapC (map (Amazonka.envRegion env,) . concat . DescribeSecurityGroups.securityGroups)

ingestSecurityGroups :: MonadIO m => Bolt.Pipe -> ConduitT (Amazonka.Region, SecurityGroup.SecurityGroup) o m ()
ingestSecurityGroups db = mapM_C ingestSecurityGroup
 where
  ingestSecurityGroup :: MonadIO m => (Amazonka.Region, SecurityGroup.SecurityGroup) -> m ()
  ingestSecurityGroup (region, group) = do
    Bolt.run db $ do
      Bolt.queryP_
        "MERGE (r:Resource {resourceARN:$r}) ON CREATE SET r:SecurityGroup, r += $g ON MATCH SET r:SecurityGroup, r += $g"
        (Bolt.props ["g" =: group, "r" =: arn])
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
   where
    arn = "arn:aws:ec2:" <> Amazonka.fromRegion region <> ":" <> SecurityGroup.ownerId group <> ":security-group/" <> SecurityGroup.groupId group

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
      Bolt.queryP_
        "MERGE (r:Resource {resourceARN:$d.dbInstanceArn}) ON CREATE SET r:DbInstance, r += $d ON MATCH SET r:DbInstance, r += $d"
        (Bolt.props ["d" =: inst])
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

fetchAllLogGroups :: MonadResource m => Amazonka.Env -> ConduitM () LogGroup.LogGroup m ()
fetchAllLogGroups env =
  Amazonka.paginate env DescribeLogGroups.newDescribeLogGroups
    .| concatMapC (concat . DescribeLogGroups.logGroups)

ingestLogGroups :: MonadIO m => Bolt.Pipe -> ConduitT LogGroup.LogGroup o m ()
ingestLogGroups db = mapM_C ingestLogGroup
 where
  ingestLogGroup :: MonadIO m => LogGroup.LogGroup -> m ()
  ingestLogGroup logGroup = do
    Bolt.run db $ do
      case LogGroup.arn logGroup of
        Just arn ->
          Bolt.queryP_
            "MERGE (r:Resource {resourceARN:$r}) ON CREATE SET r:LogGroup, r += $g ON MATCH SET r:LogGroup, r += $g"
            (Bolt.props ["r" =: arn, "g" =: logGroup])
        Nothing ->
          return ()
-}

{-
newNode :: [Label] -> Properties -> IO Node
newNode labels properties = Node <$> (Id <$> V4.nextRandom) <*> pure labels <*> pure properties

newEdge :: [Label] -> Properties -> IO Edge
newEdge labels properties = Edge <$> (Id <$> V4.nextRandom) <*> pure labels <*> pure properties

main :: IO ()
main = do
  i <- newNode ["Instance"] $ object ["instanceId" .= ("inst-0001" :: Text)]
  v1 <- newNode ["EBSVolume"] $ object ["volumeId" .= ("ebs-0001" :: Text)]
  v2 <- newNode ["EBSVolume"] $ object ["volumeId" .= ("ebs-0002" :: Text)]
  e1 <- newEdge ["InstanceVolume"] $ object ["mount" .= ("/dev/sda" :: Text)]
  e2 <- newEdge ["InstanceVolume"] $ object ["mount" .= ("/dev/sdb" :: Text)]

  let g = (i -< [e1] >- v1) <> (i -< [e2] >- v2)

  -- cfg <- Config.readConfigFile "aws-discover.yaml"
  -- lgr <- Amazonka.newLogger Amazonka.Info stdout
  -- discoveredEnv <- Amazonka.newEnv Amazonka.discover
  -- let env =
  --       discoveredEnv
  --         { Amazonka.envLogger = lgr
  --         , Amazonka.envRegion = Amazonka.Ireland
  --         }
  -- discover env (boltConfig cfg)

  -- cfg <- Config.readConfigFile "aws-discover.yaml"
  -- r <- withDb cfg $ run (insertGraph g)
  -- print r
  writeGEXF "test.gexf" g
-}
main :: IO ()
main = do
  a <- newNode
  e <- newEdge
  b <- newNode

  let g :: Graph [Int]
      g =
        a -< e >- b
          <> b -< e >- a
          & a # "A"
          & a #= ["a" .= [1]]
          & b # "B"
          & b #= ["a" .= [2]]
          & e # "E"
          & e #= ["x" .= [6]]
  -- & b
  -- `label` "B"
  -- & a
  -- `mergeProperties` props ["b" .= [2]]

  -- mapM_ (\n -> print (n, HS.toList $ labels n g, HM.toList . unProperties $ properties n g)) (nodeList g)
  mapM_ (showNode g) (nodeList g)
  mapM_ (showEdge g) (edgeList g)
 where
  showNode g n = do
    T.putStrLn $ UUID.toText (nodeId n) <> mconcat (map (" :" <>) $ HS.toList $ labels n g)
    HM.traverseWithKey (\k v -> T.putStrLn $ "  " <> k <> " = " <> T.pack (show v)) (unProperties $ properties n g)

  showEdge g (e, a, b) = do
    T.putStrLn $
      UUID.toText (nodeId a)
        <> " - "
        <> UUID.toText (edgeId e)
        <> mconcat (map (" :" <>) $ HS.toList $ labels e g)
        <> " -> "
        <> UUID.toText (nodeId b)
    HM.traverseWithKey (\k v -> T.putStrLn $ "  " <> k <> " = " <> T.pack (show v)) (unProperties $ properties e g)