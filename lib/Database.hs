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
) where

import Config
import Database.Match
import Database.Types

import Control.Monad (unless)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Functor.Contravariant ((>$))
import qualified Data.HashSet as HashSet
import Data.Text.Encoding (encodeUtf8)
import qualified Hasql.Connection as Hasql
import qualified Hasql.Decoders as D
import qualified Hasql.DynamicStatements.Session as Hasql
import qualified Hasql.DynamicStatements.Snippet as Snippet
import qualified Hasql.Encoders as E
import qualified Hasql.Session as Hasql
import qualified Hasql.Statement as Hasql

withDb :: Config -> (Hasql.Connection -> IO a) -> IO a
withDb cfg f = do
  let PostgresConfig{host, port, user, password, name} = database cfg
  connection <-
    Hasql.acquire
      ( Hasql.settings
          (encodeUtf8 host)
          port
          (encodeUtf8 user)
          (encodeUtf8 password)
          (encodeUtf8 name)
      )
  case connection of
    Left err -> error (show err)
    Right con -> do
      r <- f con
      Hasql.release con
      return r

run :: Hasql.Connection -> Db a -> IO a
run conn a = either (error . show) return =<< Hasql.run a conn

createNode :: Labels -> Properties -> Db (Id Node)
createNode labels props = Hasql.statement () statement
 where
  statement = Hasql.Statement sql encoder decoder True
  sql = "insert into nodes (labels, properties) values ($1, $2) returning id"
  encoder =
    (labels >$ E.param (E.nonNullable labelsEncoder))
      <> (props >$ E.param (E.nonNullable propertiesEncoder))
  decoder =
    D.singleRow (D.column (D.nonNullable idDecoder))

createEdge :: Labels -> Properties -> Id Node -> Id Node -> Db (Id Edge)
createEdge labels props a b = Hasql.statement () statement
 where
  statement = Hasql.Statement sql encoder decoder True
  sql = "insert into edges (labels, properties, a, b) values ($1, $2, $3, $4) returning id"
  encoder =
    (labels >$ E.param (E.nonNullable labelsEncoder))
      <> (props >$ E.param (E.nonNullable propertiesEncoder))
      <> (a >$ E.param (E.nonNullable idEncoder))
      <> (b >$ E.param (E.nonNullable idEncoder))
  decoder =
    D.singleRow (D.column (D.nonNullable idDecoder))

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

matchNode :: MatchExpr -> Db [Id Node]
matchNode expr = Hasql.dynamicallyParameterizedStatement sql decoder True
 where
  sql = "select id from nodes where " <> matchExpr expr
  decoder = D.rowList (D.column (D.nonNullable idDecoder))

matchEdge :: MatchExpr -> Maybe (Id Node) -> Maybe (Id Node) -> Db [Id Edge]
matchEdge expr a b = Hasql.dynamicallyParameterizedStatement sql decoder True
 where
  sql =
    "select id from edges where "
      <> matchExpr expr
      <> maybeNode "a" a
      <> maybeNode "b" b
  maybeNode _ Nothing = mempty
  maybeNode field (Just nodeid) = " and " <> field <> "=" <> Snippet.param nodeid
  decoder = D.rowList (D.column (D.nonNullable idDecoder))

data Merge a = Created !a | Matched !a

merged :: Merge a -> a
merged (Created a) = a
merged (Matched a) = a

mergeNode :: Labels -> Properties -> Db (Merge [Id Node])
mergeNode labels props = do
  ns <- matchNode (hasLabels labels .&. hasProperties props)
  case ns of
    [] -> do
      n <- createNode labels props
      pure (Created [n])
    xs -> pure (Matched xs)

mergeEdge :: Labels -> Properties -> Id Node -> Id Node -> Db (Merge [Id Edge])
mergeEdge labels props a b = do
  ns <- matchEdge (hasLabels labels .&. hasProperties props) (Just a) (Just b)
  case ns of
    [] -> do
      n <- createEdge labels props a b
      pure (Created [n])
    xs -> pure (Matched xs)