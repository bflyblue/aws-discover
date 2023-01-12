{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module AWS.Lambda.Functions where

import AWS.Lambda.Orphans ()
import qualified Amazonka
import qualified Amazonka.Lambda as Lambda
import qualified Amazonka.Lambda.ListFunctions as ListFunctions
import qualified Amazonka.Lambda.Types.FunctionConfiguration as FunctionConfiguration
import Conduit
import Config
import Control.Monad (void)
import Data.Aeson (KeyValue (..))
import Data.Foldable (forM_, traverse_)
import Data.Time.Clock (UTCTime)
import Database

fetchAllLambdas :: MonadResource m => Amazonka.Env -> ConduitM () Lambda.FunctionConfiguration m ()
fetchAllLambdas env =
  Amazonka.paginate env Lambda.newListFunctions
    .| concatMapC (concat . ListFunctions.functions)

ingestLambdas :: MonadIO m => Pool -> UTCTime -> ConduitT Lambda.FunctionConfiguration o m ()
ingestLambdas pool now = mapM_C ingestLambda
 where
  ingestLambda :: MonadIO m => Lambda.FunctionConfiguration -> m ()
  ingestLambda lambda = liftIO $
    run pool $ do
      r <- mergeNode ["Resource"] (properties ["resourceARN" .= FunctionConfiguration.functionArn lambda])
      addLabels ["Lambda"] (nodeId <$> merged r)
      addProperties (toProps lambda) (nodeId <$> merged r)
      case r of
        Created a -> addProperties (properties ["firstSeen" .= now]) (nodeId <$> a)
        Matched a -> addProperties (properties ["lastSeen" .= now]) (nodeId <$> a)
      traverse_
        ( \env -> do
            e <- createNode ["Environment"] (toProps env)
            forM_ (merged r) $ \r' ->
              void $ createEdge ["HAS_ENVIRONMENT"] mempty (nodeId r') (nodeId e)
        )
        (FunctionConfiguration.environment lambda)

discover :: Amazonka.Env -> Config -> UTCTime -> IO ()
discover env cfg now =
  withDb cfg $ \pool ->
    runResourceT $ runConduit $ fetchAllLambdas env .| ingestLambdas pool now
