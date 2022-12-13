{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module AWS.EC2.Vpcs where

import AWS.EC2.Orphans
import qualified Amazonka
import qualified Amazonka.EC2.DescribeVpcs as DescribeVpcs
import qualified Amazonka.EC2.Types.Vpc as Vpc
import Conduit
import Config
import Data.Aeson (KeyValue (..))
import Data.Maybe (fromMaybe)
import Database

fetchAllEc2Vpcs :: MonadResource m => Amazonka.Env -> ConduitM () (Amazonka.Region, Vpc.Vpc) m ()
fetchAllEc2Vpcs env = do
  Amazonka.paginate env DescribeVpcs.newDescribeVpcs
    .| vpcs
 where
  vpcs :: MonadResource m => ConduitM DescribeVpcs.DescribeVpcsResponse (Amazonka.Region, Vpc.Vpc) m ()
  vpcs = concatMapC $ \r ->
    maybe [] (map (Amazonka.envRegion env,)) (DescribeVpcs.vpcs r)

ingestVpcs :: MonadIO m => Connection -> ConduitT (Amazonka.Region, Vpc.Vpc) Void m ()
ingestVpcs conn = mapM_C ingestInstance
 where
  ingestInstance :: MonadIO m => (Amazonka.Region, Vpc.Vpc) -> m ()
  ingestInstance (region, vpc) = liftIO $
    run conn $ do
      r <- mergeNode ["Resource"] (properties ["resourceARN" .= arn, "region" .= region])
      addLabels ["Vpc"] r
      addProperties (toProps vpc) r
   where
    arn = "arn:aws:ec2:" <> Amazonka.fromRegion region <> ":" <> fromMaybe "" (Vpc.ownerId vpc) <> ":vpc/" <> Vpc.vpcId vpc

discover :: Amazonka.Env -> Config -> IO ()
discover env cfg = do
  r <- withDb cfg $ \conn -> do
    runResourceT $ runConduit $ fetchAllEc2Vpcs env .| ingestVpcs conn
  print r