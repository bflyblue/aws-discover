{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module AWS.EC2.Vpcs where

import AWS.EC2.Orphans ()
import qualified Amazonka
import qualified Amazonka.EC2.DescribeVpcs as DescribeVpcs
import qualified Amazonka.EC2.Types.Vpc as Vpc
import Conduit
import Config
import Control.Monad (void)
import Data.Aeson (KeyValue (..))
import Data.Maybe (fromMaybe)
import Database

fetchAllVpcs :: MonadResource m => Amazonka.Env -> ConduitM () (Amazonka.Region, Vpc.Vpc) m ()
fetchAllVpcs env = do
  Amazonka.paginate env DescribeVpcs.newDescribeVpcs
    .| vpcs
 where
  vpcs :: MonadResource m => ConduitM DescribeVpcs.DescribeVpcsResponse (Amazonka.Region, Vpc.Vpc) m ()
  vpcs = concatMapC $ \r ->
    maybe [] (map (Amazonka.envRegion env,)) (DescribeVpcs.vpcs r)

ingestVpcs :: MonadIO m => Pool -> ConduitT (Amazonka.Region, Vpc.Vpc) Void m ()
ingestVpcs pool = mapM_C ingestInstance
 where
  ingestInstance :: MonadIO m => (Amazonka.Region, Vpc.Vpc) -> m ()
  ingestInstance (region, vpc) = liftIO $
    run pool $ do
      void $ upsertNode ("arn", arn) ["Resource", "Vpc"] (toProps vpc <> metadata)
   where
    arn = "arn:aws:ec2:" <> Amazonka.fromRegion region <> ":" <> fromMaybe "" (Vpc.ownerId vpc) <> ":vpc/" <> Vpc.vpcId vpc
    metadata = properties ["resourceARN" .= arn, "region" .= region]

discover :: Amazonka.Env -> Config -> IO ()
discover env cfg =
  withDb cfg $ \pool ->
    runResourceT $ runConduit $ fetchAllVpcs env .| ingestVpcs pool