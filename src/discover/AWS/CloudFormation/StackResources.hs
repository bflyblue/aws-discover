{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module AWS.CloudFormation.StackResources where

import AWS.CloudFormation.Orphans ()
import qualified Amazonka
import qualified Amazonka.CloudFormation as CloudFormation
import qualified Amazonka.CloudFormation.ListStackResources as ListStackResources
import qualified Amazonka.CloudFormation.Types.StackResourceSummary as StackResourceSummary
import Conduit
import Config
import Data.Aeson ((.=))
import Data.Text (Text)
import Database

fetchAllStackResourceSummaries :: MonadResource m => Amazonka.Env -> (Text, Id Node) -> ConduitM () (Amazonka.Region, Text, Id Node, StackResourceSummary.StackResourceSummary) m ()
fetchAllStackResourceSummaries env (stackId, stackNode) =
  Amazonka.paginate env (CloudFormation.newListStackResources stackId)
    .| resourceSummaries
 where
  resourceSummaries :: MonadResource m => ConduitM ListStackResources.ListStackResourcesResponse (Amazonka.Region, Text, Id Node, StackResourceSummary.StackResourceSummary) m ()
  resourceSummaries = concatMapC (concatMap (map (Amazonka.region env,stackId,stackNode,)) . ListStackResources.stackResourceSummaries)

ingestStackResourceSummaries :: MonadIO m => Pool -> ConduitT (Amazonka.Region, Text, Id Node, StackResourceSummary.StackResourceSummary) o m ()
ingestStackResourceSummaries pool = mapM_C ingestStackResourceSummary
 where
  ingestStackResourceSummary :: MonadIO m => (Amazonka.Region, Text, Id Node, StackResourceSummary.StackResourceSummary) -> m ()
  ingestStackResourceSummary (region, stackId, stackNode, resource) = liftIO $
    run pool $ do
      sresId <- upsertNode ("arn", arn) ["Resource", "StackResource"] (toProps resource <> meta)
      mergeEdge_ ["HasStackResource"] (toProps resource) stackNode sresId
   where
    arn = stackId <> " " <> StackResourceSummary.logicalResourceId resource
    meta = properties ["region" .= region]

discover :: Amazonka.Env -> Config -> (Text, Id Node) -> IO ()
discover env cfg stack =
  withDb cfg $ \pool ->
    runResourceT $ runConduit $ fetchAllStackResourceSummaries env stack .| ingestStackResourceSummaries pool
