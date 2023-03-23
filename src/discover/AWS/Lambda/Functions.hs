{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
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
import Data.Aeson ((.=))
import Data.Foldable (traverse_)
import Database

fetchAllLambdas :: MonadResource m => Amazonka.Env -> ConduitM () (Amazonka.Region, Lambda.FunctionConfiguration) m ()
fetchAllLambdas env =
  Amazonka.paginate env Lambda.newListFunctions
    .| concatMapC (concatMap (map (Amazonka.region env,)) . ListFunctions.functions)

ingestLambdas :: MonadIO m => Pool -> ConduitT (Amazonka.Region, Lambda.FunctionConfiguration) o m ()
ingestLambdas pool = mapM_C ingestLambda
 where
  ingestLambda :: MonadIO m => (Amazonka.Region, Lambda.FunctionConfiguration) -> m ()
  ingestLambda (region, lambda) = liftIO $
    run pool $ do
      traverse_ (\arn' -> void $ upsertNode ("arn", arn') ["Resource", "Lambda"] (toProps lambda <> meta arn')) arn
   where
    arn = FunctionConfiguration.functionArn lambda
    meta arn' =
      properties
        [ "resourceARN" .= arn'
        , "region" .= region
        ]

discover :: Amazonka.Env -> Config -> IO ()
discover env cfg =
  withDb cfg $ \pool ->
    runResourceT $ runConduit $ fetchAllLambdas env .| ingestLambdas pool
