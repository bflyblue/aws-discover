{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module AWS.EC2.Instances where

import AWS.EC2.Orphans
import qualified Amazonka
import qualified Amazonka.EC2.DescribeInstances as DescribeInstances
import qualified Amazonka.EC2.Types.Instance as Instance
import qualified Amazonka.EC2.Types.Reservation as Reservation
import Conduit
import Config
import Data.Aeson (KeyValue (..))
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Database

fetchAllEc2Instances :: MonadResource m => Amazonka.Env -> ConduitM () (Amazonka.Region, Text, Instance.Instance) m ()
fetchAllEc2Instances env = do
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

ingestInstances :: MonadIO m => Pool -> UTCTime -> ConduitT (Amazonka.Region, Amazonka.Text, Instance.Instance) Void m ()
ingestInstances pool now = mapM_C ingestInstance
 where
  ingestInstance :: MonadIO m => (Amazonka.Region, Text, Instance.Instance) -> m ()
  ingestInstance (region, owner, inst) = liftIO $
    run pool $ do
      r <- mergeNode ["Resource"] (properties ["resourceARN" .= arn, "region" .= region, "ownerId" .= owner])
      addLabels ["Instance"] (nodeId <$> merged r)
      addProperties (toProps inst) (nodeId <$> merged r)
      case r of
        Created a -> addProperties (properties ["firstSeen" .= now]) (nodeId <$> a)
        Matched a -> addProperties (properties ["lastSeen" .= now]) (nodeId <$> a)
   where
    arn = "arn:aws:ec2:" <> Amazonka.fromRegion region <> ":" <> owner <> ":instance/" <> Instance.instanceId inst

discover :: Amazonka.Env -> Config -> UTCTime -> IO ()
discover env cfg now =
  withDb cfg $ \pool ->
    runResourceT $ runConduit $ fetchAllEc2Instances env .| ingestInstances pool now
