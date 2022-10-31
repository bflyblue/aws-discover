{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import qualified Amazonka as AWS
import qualified Amazonka.EC2 as EC2
import qualified Amazonka.EC2.DescribeInstances as EC2
import qualified Amazonka.EC2.Types.Instance as Inst
import qualified Amazonka.EC2.Types.InstanceState as InstState
import qualified Amazonka.EC2.Types.Reservation as Reservation
import Conduit
import Data.Default
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Database.Bolt ((=:))
import qualified Database.Bolt as Bolt
import System.IO (stdout)

main :: IO ()
main = do
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
        { Bolt.host = "neptune"
        , Bolt.authType = "basic"
        , Bolt.user = "neo4j"
        , Bolt.password = "darkness"
        }
  runResourceT $
    runConduit $
      fetchAllEC2Instances env .| mapM_C (injest db)
  Bolt.close db

fetchAllEC2Instances :: MonadResource m => AWS.Env -> ConduitM () EC2.Instance m ()
fetchAllEC2Instances env = AWS.paginate env EC2.newDescribeInstances .| concatMapC extractInstances

extractInstances :: EC2.DescribeInstancesResponse -> [EC2.Instance]
extractInstances = concat . mapMaybe Reservation.instances . concat . EC2.reservations

instance Bolt.IsValue EC2.Instance where
  toValue Inst.Instance'{..} =
    Bolt.toValue $
      tagMap
        <> Map.fromList
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
   where
    tagPair (EC2.Tag' k v) = k =: v
    tagMap = maybe Map.empty (Map.fromList . map tagPair) tags

injest :: MonadIO m => Bolt.Pipe -> EC2.Instance -> m ()
injest db inst =
  Bolt.run db $
    Bolt.queryP_ "CREATE (i:Instance $i)" (Bolt.props ["i" =: inst])