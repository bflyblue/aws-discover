{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Amazonka
import Amazonka.EC2 as EC2
import Amazonka.EC2.DescribeInstances
import qualified Amazonka.EC2.Types.Instance as Instance
import qualified Amazonka.EC2.Types.Reservation as Reservation
import Amazonka.ResourceGroupsTagging
import Amazonka.ResourceGroupsTagging.GetResources
import Amazonka.ResourceGroupsTagging.Types
import Conduit
import Control.Monad (void)
import Data.ByteString.Builder
import Data.List (intersperse)
import Data.Maybe (mapMaybe)
import qualified Data.Text as Text
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField (Action (..), ToField (toField))
import Database.PostgreSQL.Simple.Types (PGArray (..))
import System.IO (stdout)

main :: IO ()
main = do
  lgr <- newLogger Info stdout
  discoveredEnv <- newEnv discover
  let env =
        discoveredEnv
          { envLogger = lgr
          , envRegion = Ireland
          }
  conn <- connectPostgreSQL "postgresql://shaun:icecream@localhost/discovery"
  runResourceT $
    runConduit $
      fetchAllEC2Instances env .| mapM_C (injest conn)

injest :: MonadIO m => Connection -> Instance -> m ()
injest conn inst =
  liftIO $
    void $
      execute conn "insert into ec2_instance (ebs_optimized, tags, subnet_id, public_ip_address, public_dns_name, vpc_id) values (?,?::tag[],?,?,?,?)" (toRow inst)
 where
  toRow Instance'{..} = (ebsOptimized, PGArray . fmap toTag <$> tags, subnetId, publicIpAddress, publicDnsName, vpcId)
  toTag (EC2.Tag' k v) = Row [k, v]

data Row = forall a. ToField a => Row [a]

instance ToField Row where
  toField :: Row -> Action
  toField (Row []) = Plain (byteString "'{}'")
  toField (Row xs) =
    Many $
      Plain (byteString "ROW(")
        : (intersperse (Plain (char8 ',')) . map toField $ xs)
        ++ [Plain (char8 ')')]

ensureTag :: MonadIO m => Connection -> Text -> Text -> m Text
ensureTag conn key value = liftIO $ do
  [Only tid] <- query conn "select ensure_tag(?,?)" (key, value)
  return tid

discoverResources :: MonadResource m => Env -> m ()
discoverResources env =
  runConduit $
    fetchResourceTagMappings env .| getARNs .| discoverARN env

fetchResourceTagMappings :: MonadResource m => Env -> ConduitM () ResourceTagMapping m ()
fetchResourceTagMappings env = paginate env newGetResources .| resourceTagMappings
 where
  resourceTagMappings :: Monad m => ConduitT GetResourcesResponse ResourceTagMapping m ()
  resourceTagMappings = concatMapC (concat . resourceTagMappingList)

getARNs :: Monad m => ConduitT ResourceTagMapping Text m ()
getARNs = concatMapC resourceARN

discoverARN :: MonadResource m => Env -> ConduitT Text Void m ()
discoverARN env = mapM_C $ \arnstr ->
  case parseARN arnstr of
    Just arn
      | isAwsEC2Instance arn -> discoverEC2 env arn
    _ -> return ()

fetchAllEC2Instances :: MonadResource m => Env -> ConduitM () Instance m ()
fetchAllEC2Instances env = paginate env newDescribeInstances .| concatMapC extractInstances

fetchEC2InstanceByARN :: MonadResource m => Env -> ConduitT ARN Instance m ()
fetchEC2InstanceByARN env = mapMC describeInstanceARN .| concatMapC extractInstances
 where
  describeInstanceARN arn = send env (newDescribeInstances{instanceIds = Just [arnResourceId arn]})

extractInstances :: DescribeInstancesResponse -> [Instance]
extractInstances = concat . mapMaybe Reservation.instances . concat . reservations

discoverEC2 :: MonadResource m => Env -> ARN -> m ()
discoverEC2 env arn = do
  resp <- send env (newDescribeInstances{instanceIds = Just [arnResourceId arn]})
  liftIO $ print (maybe [] (map (maybe [] (map Instance.publicIpAddress) . Reservation.instances)) $ reservations resp)

isAwsEC2Instance :: ARN -> Bool
isAwsEC2Instance arn =
  arnPartition arn == "aws"
    && arnService arn == "ec2"
    && arnResourceType arn == Just "instance"

isAwsS3 :: ARN -> Bool
isAwsS3 arn = arnPartition arn == "aws" && arnService arn == "s3"

data ARN = ARN
  { arnPartition :: Text
  , arnService :: Text
  , arnRegion :: Text
  , arnAccountId :: Text
  , arnResourceType :: Maybe Text
  , arnResourceId :: Text
  }
  deriving (Show, Eq, Ord)

parseARN :: Text -> Maybe ARN
parseARN arn =
  case Text.splitOn ":" arn of
    [ "arn"
      , arnPartition
      , arnService
      , arnRegion
      , arnAccountId
      , resource
      ] ->
        case Text.breakOn "/" resource of
          (arnResourceId, "") ->
            Just
              ARN
                { arnPartition
                , arnService
                , arnRegion
                , arnAccountId
                , arnResourceType = Nothing
                , arnResourceId
                }
          (arnResourceType, arnResourceId) ->
            Just
              ARN
                { arnPartition
                , arnService
                , arnRegion
                , arnAccountId
                , arnResourceType = Just arnResourceType
                , arnResourceId = Text.tail arnResourceId
                }
    [ "arn"
      , arnPartition
      , arnService
      , arnRegion
      , arnAccountId
      , arnResourceId
      , arnResourceType
      ] ->
        Just
          ARN
            { arnPartition
            , arnService
            , arnRegion
            , arnAccountId
            , arnResourceType = Just arnResourceType
            , arnResourceId
            }
    _ -> Nothing
