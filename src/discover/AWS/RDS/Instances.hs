{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module AWS.RDS.Instances where

import AWS.RDS.Orphans ()
import qualified Amazonka
import qualified Amazonka.RDS.DescribeDBInstances as DescribeDbInstances
import qualified Amazonka.RDS.Types.DBInstance as DBInstance
import Conduit
import Config
import Control.Monad (void)
import Data.Aeson ((.=))
import Data.Foldable (traverse_)
import Database

fetchAllDbInstances :: MonadResource m => Amazonka.Env -> ConduitM () DBInstance.DBInstance m ()
fetchAllDbInstances env =
  Amazonka.paginate env DescribeDbInstances.newDescribeDBInstances
    .| concatMapC (concat . DescribeDbInstances.dbInstances)

ingestDbInstances :: MonadIO m => Pool -> ConduitT DBInstance.DBInstance o m ()
ingestDbInstances pool = mapM_C ingestDbInstance
 where
  ingestDbInstance :: MonadIO m => DBInstance.DBInstance -> m ()
  ingestDbInstance inst = liftIO $ do
    run pool $ do
      traverse_
        ( \arn' -> do
            void $ upsertNode ("arn", arn') ["Resource", "DbInstance"] (toProps inst <> meta)
        )
        arn
   where
    arn = DBInstance.dbInstanceArn inst
    meta =
      properties
        [ "resourceARN" .= arn
        ]

discover :: Amazonka.Env -> Config -> IO ()
discover env cfg =
  withDb cfg $ \pool ->
    runResourceT $ runConduit $ fetchAllDbInstances env .| ingestDbInstances pool
