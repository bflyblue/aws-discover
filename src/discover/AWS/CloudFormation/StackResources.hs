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
import Data.Text (Text)
import Database

fetchAllStackResourceSummaries :: MonadResource m => Amazonka.Env -> (Text, Id Node) -> ConduitM () (Text, Id Node, StackResourceSummary.StackResourceSummary) m ()
fetchAllStackResourceSummaries env (stackId, stackNode) =
  Amazonka.paginate env (CloudFormation.newListStackResources stackId)
    .| resourceSummaries
 where
  resourceSummaries :: MonadResource m => ConduitM ListStackResources.ListStackResourcesResponse (Text, Id Node, StackResourceSummary.StackResourceSummary) m ()
  resourceSummaries = concatMapC $ \r ->
    maybe [] (map (stackId,stackNode,)) (ListStackResources.stackResourceSummaries r)

ingestStackResourceSummaries :: MonadIO m => Pool -> ConduitT (Text, Id Node, StackResourceSummary.StackResourceSummary) o m ()
ingestStackResourceSummaries pool = mapM_C ingestStackResourceSummary
 where
  ingestStackResourceSummary :: MonadIO m => (Text, Id Node, StackResourceSummary.StackResourceSummary) -> m ()
  ingestStackResourceSummary (stackId, stackNode, resource) = liftIO $
    run pool $ do
      sresId <- upsertNode ("arn", arn) ["Resource", "StackResource"] (toProps resource)
      mergeEdge_ ["HasStackResource"] (toProps resource) stackNode sresId
   where
    arn = stackId <> " " <> StackResourceSummary.logicalResourceId resource

discover :: Amazonka.Env -> Config -> (Text, Id Node) -> IO ()
discover env cfg stack =
  withDb cfg $ \pool ->
    runResourceT $ runConduit $ fetchAllStackResourceSummaries env stack .| ingestStackResourceSummaries pool
