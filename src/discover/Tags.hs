{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Tags where

import Conduit
import Config
import Data.Aeson ((.=))
import Data.Conduit.List (sourceList)
import Data.Text (Text)
import Database
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Session as Hasql
import qualified Hasql.Statement as Hasql

data Tag = Tag Text Text

findTagged :: MonadResource m => Pool -> Text -> ConduitM () (Id Node, Tag) m ()
findTagged pool tag = do
  tagged <- liftIO $ run pool $ Hasql.statement tag statement
  sourceList tagged
 where
  statement = Hasql.Statement sql encoder decoder True
  sql = "select id, value from tags where key = $1"
  encoder = E.param (E.nonNullable E.text)
  decoder = D.rowList ((\n v -> (n, Tag tag v)) <$> D.column (D.nonNullable idDecoder) <*> D.column (D.nonNullable D.text))

relateTagged :: MonadIO m => Pool -> Text -> Text -> ConduitT (Id Node, Tag) o m ()
relateTagged pool tag label = mapM_C relate
 where
  relate :: MonadIO m => (Id Node, Tag) -> m ()
  relate (nodeid, Tag _ val) = liftIO $
    run pool $ do
      tagId <- upsertNode ("tag", tag <> ":" <> val) [tag] (properties ["name" .= val])
      mergeEdge_ [label] mempty nodeid tagId

discover :: Config -> Text -> Text -> Text -> IO ()
discover cfg key tag label =
  withDb cfg $ \pool ->
    runResourceT $ runConduit $ findTagged pool key .| relateTagged pool tag label
