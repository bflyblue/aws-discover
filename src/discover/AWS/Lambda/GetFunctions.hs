{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module AWS.Lambda.GetFunctions where

import AWS.Lambda.Orphans ()
import qualified Amazonka
import qualified Amazonka.Lambda as Lambda
import qualified Amazonka.Lambda.GetFunction as GetFunction
import qualified Amazonka.Lambda.Types.FunctionCodeLocation as FunctionCodeLocation
import Conduit
import Config
import qualified Control.Monad.Catch as Catch
import qualified Data.Conduit.Binary as CB
import Data.Conduit.List (sourceList)
import Data.Text (Text)
import qualified Data.Text as Text
import Database
import qualified Hasql.Decoders as D
import qualified Hasql.Session as Hasql
import qualified Hasql.Statement as Hasql
import Network.HTTP.Req
import Network.HTTP.Req.Conduit
import Text.URI

findFunctions :: MonadResource m => Pool -> ConduitM () (Text, Text) m ()
findFunctions pool = do
  arns <- liftIO $ run pool $ Hasql.statement () statement
  sourceList arns
 where
  statement = Hasql.Statement sql encoder decoder True
  sql = "select arn, \"functionName\" from lambdas"
  encoder = mempty
  decoder = D.rowList ((,) <$> D.column (D.nonNullable D.text) <*> D.column (D.nonNullable D.text))

getLocations :: (Catch.MonadCatch m, MonadResource m) => Amazonka.Env -> ConduitM (Text, Text) (Text, Text, Text) m ()
getLocations env = concatMapMC $ \(arn, functionName) -> do
  location <- getLocation arn
  return $ (arn,functionName,) <$> location
 where
  getLocation arn = do
    -- resp <- Amazonka.send env (Lambda.newGetFunction arn)
    resp <-
      (Just <$> Amazonka.send env (Lambda.newGetFunction arn))
        `Catch.catch` (\(Amazonka.ServiceError se) -> liftIO (print se) >> return Nothing)
    liftIO (print resp)
    return (resp >>= GetFunction.code >>= FunctionCodeLocation.location)

sinkFunctions :: MonadResource m => ConduitM (Text, Text, Text) o m ()
sinkFunctions = mapM_C get
 where
  get (_arn, functionName, location) = do
    runReq defaultHttpConfig $ do
      uri <- mkURI location
      case useURI uri of
        Just (Left (httpUri, opts)) ->
          reqBr GET httpUri NoReqBody opts handle
        Just (Right (httpsUri, opts)) ->
          reqBr GET httpsUri NoReqBody opts handle
        Nothing ->
          error $ "invalid uri: " <> show location
   where
    handle r = runConduitRes $ responseBodySource r .| CB.sinkFile filename
    filename = Text.unpack functionName

getFunctions :: Amazonka.Env -> Config -> IO ()
getFunctions env cfg =
  withDb cfg $ \pool ->
    runResourceT $ runConduit $ findFunctions pool .| getLocations env .| sinkFunctions
