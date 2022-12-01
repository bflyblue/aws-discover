{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module AWS.Instances where

-- import qualified Amazonka.EC2.DescribeSecurityGroups as DescribeSecurityGroups
-- import qualified Amazonka.EC2.DescribeSubnets as DescribeSubnets
-- import qualified Amazonka.EC2.DescribeVpcs as DescribeVpcs
-- import qualified Amazonka.EC2.Types.SecurityGroup as SecurityGroup
-- import qualified Amazonka.EC2.Types.Subnet as Subnet
-- import qualified Amazonka.EC2.Types.Vpc as Vpc

import qualified Amazonka
import qualified Amazonka.EC2.DescribeInstances as DescribeInstances
import qualified Amazonka.EC2.Types.Instance as Instance
import qualified Amazonka.EC2.Types.Reservation as Reservation
import Conduit
import Data.Text (Text)
import qualified Data.UUID.V4 as V4
import Database.Types

discover :: Amazonka.Env -> IO Graph
discover env = do
  n' <- node <$> (Id <$> V4.nextRandom)
  e' <- edge <$> (Id <$> V4.nextRandom)
  let a = n' .: "Instance" .= mempty
      b = n' .: "Instance" .= mempty
      e = e' .: "ChildOf" .= mempty
  return $ relate a e b

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