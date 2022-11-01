{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import qualified Amazonka as AWS
import qualified Amazonka.EC2 as EC2
import qualified Amazonka.EC2.DescribeInstances as EC2
import qualified Amazonka.EC2.DescribeVpcs as EC2
import qualified Amazonka.EC2.Types.Instance as Inst
import qualified Amazonka.EC2.Types.InstanceState as InstState
import qualified Amazonka.EC2.Types.Reservation as Reservation
import qualified Amazonka.EC2.Types.Vpc as Vpc
import qualified Amazonka.EC2.Types.VpcCidrBlockAssociation as VpcCidr
import Conduit
import Data.Default
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Yaml as Yaml
import Database.Bolt ((=:))
import qualified Database.Bolt as Bolt
import GHC.Generics (Generic)
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
  db <-
    Bolt.connect
      def
        { Bolt.host = host cfg
        , Bolt.user = user cfg
        , Bolt.password = password cfg
        }
  runResourceT $ discover env db
  Bolt.close db

discover :: MonadResource m => AWS.Env -> Bolt.Pipe -> m ()
discover env db = do
  Bolt.run db $ do
    Bolt.query_ "MATCH (i:Instance) DETACH DELETE i"
    Bolt.query_ "MATCH (v:Vpc) DETACH DELETE v"

  runConduit $ fetchAllEc2Instances env .| mapM_C (injestInstance db)
  runConduit $ fetchAllVpcs env .| mapM_C (injestVpc db)

fetchAllEc2Instances :: MonadResource m => AWS.Env -> ConduitM () EC2.Instance m ()
fetchAllEc2Instances env = AWS.paginate env EC2.newDescribeInstances .| concatMapC extractInstances

extractInstances :: EC2.DescribeInstancesResponse -> [EC2.Instance]
extractInstances = concat . mapMaybe Reservation.instances . concat . EC2.reservations

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

injestInstance :: MonadIO m => Bolt.Pipe -> EC2.Instance -> m ()
injestInstance db inst =
  Bolt.run db $ do
    Bolt.queryP_ "CREATE (i:Instance $i)" (Bolt.props ["i" =: inst])
    traverse_
      ( \tags ->
          Bolt.queryP_
            "MATCH (i) WHERE i.instanceId = $i CREATE (i)-[:HAS_TAGS]->(t:Tags $t)"
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

injestVpc :: MonadIO m => Bolt.Pipe -> EC2.Vpc -> m ()
injestVpc db vpc =
  Bolt.run db $ do
    Bolt.queryP_ "CREATE (v:Vpc $v)" (Bolt.props ["v" =: vpc])
    traverse_
      ( \tags ->
          Bolt.queryP_
            "MATCH (v) WHERE v.vpcId = $v CREATE (v)-[:HAS_TAGS]->(t:Tags $t)"
            (Bolt.props ["v" =: Vpc.vpcId vpc, "t" =: Tags tags])
      )
      (Vpc.tags vpc)
    (traverse_ . traverse_)
      ( \cidr ->
          Bolt.queryP_
            "MATCH (v) WHERE v.vpcId = $v CREATE (v)-[:HAS_TAGS]->(t:Tags $t)"
            (Bolt.props ["v" =: Vpc.vpcId vpc, "c" =: cidr])
      )
      (Vpc.cidrBlockAssociationSet vpc)