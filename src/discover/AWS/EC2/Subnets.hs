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
import Data.Aeson (KeyValue (..))
import Data.Time.Clock (UTCTime)
import Database

fetchAllSubnets :: MonadResource m => Amazonka.Env -> ConduitM () (Amazonka.Region, Subnet.Subnet) m ()
fetchAllSubnets env = do
  Amazonka.paginate env DescribeSubnets.newDescribeSubnets
    .| concatMapC (map (Amazonka.envRegion env,) . concat . DescribeSubnets.subnets)

ingestSubnets :: MonadIO m => Pool -> UTCTime -> ConduitT (Amazonka.Region, Subnet.Subnet) Void m ()
ingestSubnets pool now = mapM_C ingestSubnet
 where
  ingestSubnet :: MonadIO m => (Amazonka.Region, Subnet.Subnet) -> m ()
  ingestSubnet (region, subnet) = liftIO $
    run pool $ do
      r <- mergeNode ["Resource"] (properties ["resourceARN" .= Subnet.subnetArn subnet, "ownerId" .= Subnet.ownerId subnet, "region" .= region])
      addLabels ["Subnet"] (nodeId <$> merged r)
      addProperties (toProps subnet) (nodeId <$> merged r)
      case r of
        Created a -> addProperties (properties ["firstSeen" .= now]) (nodeId <$> a)
        Matched a -> addProperties (properties ["lastSeen" .= now]) (nodeId <$> a)

discover :: Amazonka.Env -> Config -> UTCTime -> IO ()
discover env cfg now =
  withDb cfg $ \pool ->
    runResourceT $ runConduit $ fetchAllSubnets env .| ingestSubnets pool now