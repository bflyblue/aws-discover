{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module AWS.EC2.SecurityGroups where

import AWS.EC2.Orphans ()
import qualified Amazonka
import qualified Amazonka.EC2.DescribeSecurityGroups as DescribeSecurityGroups
import qualified Amazonka.EC2.Types.SecurityGroup as SecurityGroup
import Conduit
import Config
import Control.Monad (void)
import Data.Aeson (KeyValue (..))
import Database

fetchAllSecurityGroups :: MonadResource m => Amazonka.Env -> ConduitM () (Amazonka.Region, SecurityGroup.SecurityGroup) m ()
fetchAllSecurityGroups env = do
  Amazonka.paginate env DescribeSecurityGroups.newDescribeSecurityGroups
    .| concatMapC (map (Amazonka.region env,) . concat . DescribeSecurityGroups.securityGroups)

ingestSecurityGroups :: MonadIO m => Pool -> ConduitT (Amazonka.Region, SecurityGroup.SecurityGroup) Void m ()
ingestSecurityGroups pool = mapM_C ingestSecurityGroup
 where
  ingestSecurityGroup :: MonadIO m => (Amazonka.Region, SecurityGroup.SecurityGroup) -> m ()
  ingestSecurityGroup (region, group) = liftIO $
    run pool $ do
      void $ upsertNode ("arn", arn) ["Resource", "SecurityGroup"] (toProps group <> metadata)
   where
    arn = "arn:aws:ec2:" <> Amazonka.fromRegion region <> ":" <> SecurityGroup.ownerId group <> ":security-group/" <> SecurityGroup.groupId group
    metadata = properties ["resourceARN" .= arn, "ownerId" .= SecurityGroup.ownerId group, "region" .= region]

discover :: Amazonka.Env -> Config -> IO ()
discover env cfg =
  withDb cfg $ \pool ->
    runResourceT $ runConduit $ fetchAllSecurityGroups env .| ingestSecurityGroups pool