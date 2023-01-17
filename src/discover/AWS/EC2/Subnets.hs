{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module AWS.EC2.Subnets where

import AWS.EC2.Orphans ()
import qualified Amazonka
import qualified Amazonka.EC2.DescribeSubnets as DescribeSubnets
import qualified Amazonka.EC2.Types.Subnet as Subnet
import Conduit
import Config
import Control.Monad (void)
import Data.Aeson (KeyValue (..))
import Data.Foldable (traverse_)
import Database

fetchAllSubnets :: MonadResource m => Amazonka.Env -> ConduitM () (Amazonka.Region, Subnet.Subnet) m ()
fetchAllSubnets env = do
  Amazonka.paginate env DescribeSubnets.newDescribeSubnets
    .| concatMapC (map (Amazonka.envRegion env,) . concat . DescribeSubnets.subnets)

ingestSubnets :: MonadIO m => Pool -> ConduitT (Amazonka.Region, Subnet.Subnet) Void m ()
ingestSubnets pool = mapM_C ingestSubnet
 where
  ingestSubnet :: MonadIO m => (Amazonka.Region, Subnet.Subnet) -> m ()
  ingestSubnet (region, subnet) = liftIO $
    run pool $ do
      traverse_ (\arn' -> void $ upsertNode ("arn", arn') ["Resource", "Subnet"] (toProps subnet <> metadata)) arn
   where
    arn = Subnet.subnetArn subnet
    metadata = properties ["resourceARN" .= arn, "ownerId" .= Subnet.ownerId subnet, "region" .= region]

discover :: Amazonka.Env -> Config -> IO ()
discover env cfg =
  withDb cfg $ \pool ->
    runResourceT $ runConduit $ fetchAllSubnets env .| ingestSubnets pool