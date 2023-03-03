{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module AWS.SecretsManager.Secrets where

import AWS.SecretsManager.Orphans ()
import qualified Amazonka
import qualified Amazonka.SecretsManager as SecretsManager
import qualified Amazonka.SecretsManager.GetSecretValue as GetSecretValue
import Conduit
import Config
import Control.Applicative ((<|>))
import qualified Control.Monad.Catch as Catch
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Conduit.List (sourceList)
import Data.Foldable (for_)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Database
import qualified Hasql.Decoders as D
import qualified Hasql.Session as Hasql
import qualified Hasql.Statement as Hasql

findEnvReferredSecrets :: MonadResource m => Pool -> ConduitM () (Id Node, Text) m ()
findEnvReferredSecrets pool = do
  arns <- liftIO $ run pool $ Hasql.statement () statement
  sourceList arns
 where
  statement = Hasql.Statement sql encoder decoder True
  sql = "select id, value->>0 from environments where jsonb_typeof(value)='string' and value->>0 like 'arn:aws:secretsmanager%'"
  encoder = mempty
  decoder = D.rowList ((,) <$> D.column (D.nonNullable idDecoder) <*> D.column (D.nonNullable D.text))

fetchSecrets :: (Catch.MonadCatch m, MonadResource m) => Amazonka.Env -> ConduitM (Id Node, Text) (Id Node, SecretsManager.GetSecretValueResponse) m ()
fetchSecrets env = concatMapMC $ \(nodeId, arn) -> do
  secret <- fetch arn
  return $ (nodeId,) <$> secret
 where
  fetch arn =
    (Just <$> Amazonka.send env (SecretsManager.newGetSecretValue arn))
      `Catch.catch` (\(Amazonka.ServiceError se) -> liftIO (print se) >> return Nothing)

ingestSecrets :: MonadIO m => Pool -> ConduitT (Id Node, SecretsManager.GetSecretValueResponse) o m ()
ingestSecrets pool = mapM_C ingestSecretValueResponse
 where
  ingestSecretValueResponse :: MonadIO m => (Id Node, SecretsManager.GetSecretValueResponse) -> m ()
  ingestSecretValueResponse (nodeId, resp) = liftIO $
    run pool $ do
      for_ (GetSecretValue.arn resp) $ \arn' -> do
        secretId <- upsertNode ("arn", arn') ["Resource", "Secrets"] (meta resp <> secrets resp)
        mergeEdge_ ["EnvRefersTo"] (properties []) nodeId secretId
   where
    meta GetSecretValue.GetSecretValueResponse'{..} =
      properties
        [ "arn" .= arn
        , "resourceARN" .= arn
        , "createdDate" .= createdDate
        , "name" .= name
        , "versionId" .= versionId
        , "versionStages" .= versionStages
        ]

  secrets :: GetSecretValue.GetSecretValueResponse -> Properties
  secrets resp =
    case secret of
      Just (Aeson.Object o) ->
        properties ["secrets" .= KeyMap.mapMaybeWithKey clean o]
      Just _ ->
        properties ["secrets" .= hidden]
      _ ->
        mempty
   where
    secret =
      Aeson.decodeStrict
        =<< Text.encodeUtf8 . Amazonka.fromSensitive <$> GetSecretValue.secretString resp
          <|> Amazonka.unBase64 . Amazonka.fromSensitive <$> GetSecretValue.secretBinary resp

  hidden :: Text
  hidden = "****"

  clean :: Key.Key -> Aeson.Value -> Maybe Aeson.Value
  clean key val
    | safe key val = Just val
    | otherwise = Just $ Aeson.String hidden

  safe :: Key.Key -> Aeson.Value -> Bool
  safe key val = safeKey key || safeVal val

  safeKey key = any @[] (`Text.isSuffixOf` Key.toText key) ["_DB_ENGINE", "_DB_HOST", "_DB_NAME", "_DB_PORT"]

  safeVal (Aeson.String val) = "rds.amazonaws.com" `Text.isSuffixOf` val
  safeVal _ = False

discover :: Amazonka.Env -> Config -> IO ()
discover env cfg =
  withDb cfg $ \pool ->
    runResourceT $ runConduit $ findEnvReferredSecrets pool .| fetchSecrets env .| ingestSecrets pool
