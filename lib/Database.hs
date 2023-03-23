{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Database (
  module Database,
  module Database.Match,
  module Database.Types,
  Hasql.Connection,
  Pool.Pool,
) where

import Config
import Database.Match
import Database.Types

import Control.Monad (unless, void)
import Data.Aeson (FromJSON, Result (..), ToJSON (toJSON), Value (Object), fromJSON)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Functor.Contravariant ((>$))
import qualified Data.HashSet as HashSet
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Hasql.Connection as Hasql
import qualified Hasql.Decoders as D
import qualified Hasql.DynamicStatements.Session as Hasql
import qualified Hasql.DynamicStatements.Snippet as Snippet
import qualified Hasql.Encoders as E
import qualified Hasql.Pool as Pool
import qualified Hasql.Session as Hasql
import qualified Hasql.Statement as Hasql

withDb :: Config -> (Pool.Pool -> IO a) -> IO a
withDb cfg f = do
  let PostgresConfig{host, port, user, password, name} = database cfg
      settings =
        Hasql.settings
          (encodeUtf8 host)
          port
          (encodeUtf8 user)
          (encodeUtf8 password)
          (encodeUtf8 name)
  pool <- Pool.acquire 8 (Just 1000000) settings
  f pool

run :: Pool.Pool -> Db a -> IO a
run pool a = either (error . show) return =<< Pool.use pool a

createNode :: Labels -> Properties -> Db Node
createNode lbls prop = Hasql.statement () statement
 where
  statement = Hasql.Statement sql encoder decoder True
  sql = "insert into nodes (labels, properties) values ($1, $2) returning row(id, labels, properties)"
  encoder =
    (lbls >$ E.param (E.nonNullable labelsEncoder))
      <> (prop >$ E.param (E.nonNullable propertiesEncoder))
  decoder =
    D.singleRow (D.column (D.nonNullable nodeDecoder))

createEdge :: Labels -> Properties -> Id Node -> Id Node -> Db Edge
createEdge lbls prop a b = Hasql.statement () statement
 where
  statement = Hasql.Statement sql encoder decoder True
  sql = "insert into edges (labels, properties, a, b) values ($1, $2, $3, $4) returning row(id, labels, properties, a, b)"
  encoder =
    (lbls >$ E.param (E.nonNullable labelsEncoder))
      <> (prop >$ E.param (E.nonNullable propertiesEncoder))
      <> (a >$ E.param (E.nonNullable idEncoder))
      <> (b >$ E.param (E.nonNullable idEncoder))
  decoder =
    D.singleRow (D.column (D.nonNullable edgeDecoder))

addLabels :: forall a. HasTable a => Labels -> [Id a] -> Db ()
addLabels lbls nodes =
  unless (HashSet.null $ unLabels lbls) $ Hasql.statement () statement
 where
  statement = Hasql.Statement sql encoder decoder True
  sql = "update " <> tableName @a <> " set labels = array(select distinct unnest(labels || $1)) where id = any($2)"
  encoder =
    (lbls >$ E.param (E.nonNullable labelsEncoder))
      <> (nodes >$ E.param (E.nonNullable (E.foldableArray (E.nonNullable idEncoder))))
  decoder = D.noResult

addProperties :: forall a. HasTable a => Properties -> [Id a] -> Db ()
addProperties prop nodes =
  unless (KeyMap.null $ unProperties prop) $ Hasql.statement () statement
 where
  statement = Hasql.Statement sql encoder decoder True
  sql = "update " <> tableName @a <> " set properties = properties || $1 where id = any($2)"
  encoder =
    (prop >$ E.param (E.nonNullable propertiesEncoder))
      <> (nodes >$ E.param (E.nonNullable (E.foldableArray (E.nonNullable idEncoder))))
  decoder = D.noResult

matchNode :: Match Bool -> Db [Node]
matchNode expr = Hasql.dynamicallyParameterizedStatement sql decoder True
 where
  sql = "select row(id, labels, properties) from nodes where " <> match expr
  decoder = D.rowList (D.column (D.nonNullable nodeDecoder))

getNode :: Id Node -> Db Node
getNode nodeid = Hasql.statement nodeid statement
 where
  statement = Hasql.Statement sql encoder decoder True
  sql = "select row(id, labels, properties) from nodes where id = $1"
  encoder = E.param (E.nonNullable idEncoder)
  decoder = D.singleRow (D.column (D.nonNullable nodeDecoder))

matchEdge :: Match Bool -> Maybe (Id Node) -> Maybe (Id Node) -> Db [Edge]
matchEdge expr a b = Hasql.dynamicallyParameterizedStatement sql decoder True
 where
  sql =
    "select row(id, labels, properties, a, b) from edges where "
      <> match expr
      <> maybeNode "a" a
      <> maybeNode "b" b
  maybeNode _ Nothing = mempty
  maybeNode field (Just nodeid) = " and " <> field <> "=" <> Snippet.param nodeid
  decoder = D.rowList (D.column (D.nonNullable edgeDecoder))

data Merge a = Created !a | Matched !a

merged :: Merge a -> a
merged (Created a) = a
merged (Matched a) = a

mergeNode :: Labels -> Properties -> Db (Merge [Node])
mergeNode lbls prop = do
  ns <- matchNode (hasLabels lbls .&. hasProperties prop)
  case ns of
    [] -> do
      n <- createNode lbls prop
      pure (Created [n])
    xs -> pure (Matched xs)

mergeEdge :: Labels -> Properties -> Id Node -> Id Node -> Db (Merge [Edge])
mergeEdge lbls prop a b = do
  ns <- matchEdge (hasLabels lbls .&. hasProperties prop) (Just a) (Just b)
  case ns of
    [] -> do
      n <- createEdge lbls prop a b
      pure (Created [n])
    xs -> pure (Matched xs)

mergeEdge_ :: Labels -> Properties -> Id Node -> Id Node -> Hasql.Session ()
mergeEdge_ lbls prop a b = void $ mergeEdge lbls prop a b

upsertNode :: (Text, Text) -> Labels -> Properties -> Db (Id Node)
upsertNode (keyspace, key) lbls prop = Hasql.statement () statement
 where
  statement = Hasql.Statement sql encoder decoder True
  sql = "insert into nodes (keyspace, key, labels, properties) values ($1, $2, $3, $4) on conflict (keyspace, key) do update set labels = array(select distinct unnest(nodes.labels || EXCLUDED.labels)), properties = nodes.properties || EXCLUDED.properties returning id"
  encoder =
    (keyspace >$ E.param (E.nonNullable E.text))
      <> (key >$ E.param (E.nonNullable E.text))
      <> (lbls >$ E.param (E.nonNullable labelsEncoder))
      <> (prop >$ E.param (E.nonNullable propertiesEncoder))
  decoder =
    D.singleRow (D.column (D.nonNullable idDecoder))

toProps :: ToJSON a => a -> Properties
toProps a = case toJSON a of
  Object o -> Properties o
  _ -> error "expected Object"

getProperty :: Text -> Properties -> Maybe Value
getProperty k (Properties p) = KeyMap.lookup (Key.fromText k) p

labelIn :: Text -> Labels -> Bool
labelIn k l = k `HashSet.member` unLabels l

decodeProperty :: FromJSON a => Text -> Properties -> Maybe a
decodeProperty k p =
  getProperty k p
    >>= \v -> case fromJSON v of
      Success a -> Just a
      Error _ -> Nothing