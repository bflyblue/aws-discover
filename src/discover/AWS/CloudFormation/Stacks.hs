{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module AWS.CloudFormation.Stacks where

import AWS.CloudFormation.Orphans ()
import qualified AWS.CloudFormation.StackResources as StackResources
import qualified Amazonka
import qualified Amazonka.CloudFormation as CloudFormation
import qualified Amazonka.CloudFormation.ListStacks as ListStacks
import qualified Amazonka.CloudFormation.Types.StackSummary as StackSummary
import Conduit
import Config
import Control.Monad (void)
import Data.Aeson ((.=))
import Data.Text (Text)
import Data.Traversable (for)
import Database

fetchAllStackSummaries :: MonadResource m => Amazonka.Env -> ConduitM () (Amazonka.Region, StackSummary.StackSummary) m ()
fetchAllStackSummaries env =
  Amazonka.paginate env CloudFormation.newListStacks
    .| concatMapC (concatMap (map (Amazonka.region env,)) . ListStacks.stackSummaries)

ingestStackSummaries :: MonadIO m => Pool -> ConduitT (Amazonka.Region, StackSummary.StackSummary) (Text, Id Node) m ()
ingestStackSummaries pool = concatMapMC ingestStackSummary
 where
  ingestStackSummary :: MonadIO m => (Amazonka.Region, StackSummary.StackSummary) -> m (Maybe (Text, Id Node))
  ingestStackSummary (region, stack) = liftIO $
    run pool $ do
      for arn $ \arn' -> do
        node <- upsertNode ("arn", arn') ["Resource", "Stack"] (toProps stack <> meta arn')
        return (arn', node)
   where
    arn = StackSummary.stackId stack
    meta arn' =
      properties
        [ "resourceARN" .= arn'
        , "region" .= region
        ]

discoverStackResources :: MonadIO m => Amazonka.Env -> Config -> ConduitT (Text, Id Node) o m ()
discoverStackResources env cfg = mapM_C (liftIO . StackResources.discover env cfg)

discover :: Amazonka.Env -> Config -> IO ()
discover env cfg =
  withDb cfg $ \pool ->
    runResourceT $
      void $
        runConduit $
          fetchAllStackSummaries env
            .| ingestStackSummaries pool
            .| discoverStackResources env cfg
