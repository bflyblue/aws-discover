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

import Control.Monad (unless)
import Data.Aeson (ToJSON (toJSON), Value (Object))
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Functor.Contravariant ((>$))
import qualified Data.HashSet as HashSet
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
createNode labels props = Hasql.statement () statement
 where
  statement = Hasql.Statement sql encoder decoder True
  sql = "insert into nodes (labels, properties) values ($1, $2) returning row(nodes.*)"
  encoder =
    (labels >$ E.param (E.nonNullable labelsEncoder))
      <> (props >$ E.param (E.nonNullable propertiesEncoder))
  decoder =
    D.singleRow (D.column (D.nonNullable nodeDecoder))

createEdge :: Labels -> Properties -> Id Node -> Id Node -> Db Edge
createEdge labels props a b = Hasql.statement () statement
 where
  statement = Hasql.Statement sql encoder decoder True
  sql = "insert into edges (labels, properties, a, b) values ($1, $2, $3, $4) returning row(edges.*)"
  encoder =
    (labels >$ E.param (E.nonNullable labelsEncoder))
      <> (props >$ E.param (E.nonNullable propertiesEncoder))
      <> (a >$ E.param (E.nonNullable idEncoder))
      <> (b >$ E.param (E.nonNullable idEncoder))
  decoder =
    D.singleRow (D.column (D.nonNullable edgeDecoder))

addLabels :: forall a. HasTable a => Labels -> [Id a] -> Db ()
addLabels labels nodes =
  unless (HashSet.null $ unLabels labels) $ Hasql.statement () statement
 where
  statement = Hasql.Statement sql encoder decoder True
  sql = "update " <> tableName @a <> " set labels = array(select distinct unnest(labels || $1)) where id = any($2)"
  encoder =
    (labels >$ E.param (E.nonNullable labelsEncoder))
      <> (nodes >$ E.param (E.nonNullable (E.foldableArray (E.nonNullable idEncoder))))
  decoder = D.noResult

addProperties :: forall a. HasTable a => Properties -> [Id a] -> Db ()
addProperties props nodes =
  unless (KeyMap.null $ unProperties props) $ Hasql.statement () statement
 where
  statement = Hasql.Statement sql encoder decoder True
  sql = "update " <> tableName @a <> " set properties = properties || $1 where id = any($2)"
  encoder =
    (props >$ E.param (E.nonNullable propertiesEncoder))
      <> (nodes >$ E.param (E.nonNullable (E.foldableArray (E.nonNullable idEncoder))))
  decoder = D.noResult

matchNode :: MatchExpr -> Db [Node]
matchNode expr = Hasql.dynamicallyParameterizedStatement sql decoder True
 where
  sql = "select row(nodes.*) from nodes where " <> matchExpr expr
  decoder = D.rowList (D.column (D.nonNullable nodeDecoder))

matchEdge :: MatchExpr -> Maybe (Id Node) -> Maybe (Id Node) -> Db [Edge]
matchEdge expr a b = Hasql.dynamicallyParameterizedStatement sql decoder True
 where
  sql =
    "select row(edges.*) from edges where "
      <> matchExpr expr
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
mergeNode labels props = do
  ns <- matchNode (hasLabels labels .&. hasProperties props)
  case ns of
    [] -> do
      n <- createNode labels props
      pure (Created [n])
    xs -> pure (Matched xs)

mergeEdge :: Labels -> Properties -> Id Node -> Id Node -> Db (Merge [Edge])
mergeEdge labels props a b = do
  ns <- matchEdge (hasLabels labels .&. hasProperties props) (Just a) (Just b)
  case ns of
    [] -> do
      n <- createEdge labels props a b
      pure (Created [n])
    xs -> pure (Matched xs)

toProps :: ToJSON a => a -> Properties
toProps a = case toJSON a of
  Object o -> Properties o
  _ -> error "expected Object"