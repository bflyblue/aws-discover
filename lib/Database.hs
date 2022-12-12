{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Database where

import Config

import Control.Monad (unless)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types
import Data.ByteString (ByteString)
import Data.Functor.Contravariant ((>$), (>$<))
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Int
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Hasql.Connection as Hasql
import qualified Hasql.Decoders as D
import qualified Hasql.DynamicStatements.Session as Hasql
import qualified Hasql.DynamicStatements.Snippet as Snippet
import qualified Hasql.Encoders as E
import Hasql.Implicits.Encoders
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

type Label = Text

newtype Labels = Labels {unLabels :: HashSet Label}
  deriving (Show, Eq, Ord, Semigroup, Monoid)

newtype Properties = Properties {unProperties :: Object}
  deriving (Show, Eq, Ord, Semigroup, Monoid)

data Node = Node
  { nodeId :: Id Node
  , nodeLabels :: Labels
  , nodeProperties :: Properties
  }
  deriving (Show)

data Edge = Edge
  { edgeId :: Id Edge
  , edgeLabels :: Labels
  , edgeProperties :: Properties
  , edgeA :: Id Node
  , edgeB :: Id Node
  }
  deriving (Show)

newtype Id a = Id {unId :: Int32}
  deriving (Show, Eq, Ord)

class HasTable a where
  tableName :: ByteString

instance HasTable Node where
  tableName = "nodes"

instance HasTable Edge where
  tableName = "edges"

data MatchExpr
  = HasLabels Labels
  | HasProperties Properties
  | PropCmp Text Cmp Value
  | MatchAnd MatchExpr MatchExpr
  | MatchOr MatchExpr MatchExpr
  | MatchNot MatchExpr

hasLabels :: Labels -> MatchExpr
hasLabels = HasLabels

hasLabel :: Label -> MatchExpr
hasLabel a = HasLabels $ mkLabels [a]

hasProperties :: Properties -> MatchExpr
hasProperties = HasProperties

(.=), (.!=), (.<), (.<=), (.>), (.>=) :: Text -> Value -> MatchExpr
f .= v = PropCmp f CmpEq v
f .!= v = PropCmp f CmpNeq v
f .< v = PropCmp f CmpLt v
f .<= v = PropCmp f CmpLte v
f .> v = PropCmp f CmpGt v
f .>= v = PropCmp f CmpGte v

infix 4 .=, .!=, .<, .<=, .>, .>=

(.&), (.|) :: MatchExpr -> MatchExpr -> MatchExpr
(.&) = MatchAnd
(.|) = MatchOr

infixl 3 .&
infixl 2 .|

not :: MatchExpr -> MatchExpr
not = MatchNot

matchExpr :: MatchExpr -> Snippet.Snippet
matchExpr (HasLabels labels) = "(labels @> " <> Snippet.param labels <> ")"
matchExpr (HasProperties props) = "(properties @> " <> Snippet.param props <> ")"
matchExpr (PropCmp field op val) = "(properties->" <> Snippet.param field <> cmpExpr op <> Snippet.param val <> ")"
matchExpr (MatchAnd a b) = "(" <> matchExpr a <> " and " <> matchExpr b <> ")"
matchExpr (MatchOr a b) = "(" <> matchExpr a <> " or " <> matchExpr b <> ")"
matchExpr (MatchNot a) = "not(" <> matchExpr a <> ")"

cmpExpr :: Cmp -> Snippet.Snippet
cmpExpr CmpEq = "="
cmpExpr CmpGt = "<"
cmpExpr CmpLt = ">"
cmpExpr CmpGte = ">="
cmpExpr CmpLte = "<="
cmpExpr CmpNeq = "!="

data Cmp = CmpEq | CmpLt | CmpGt | CmpLte | CmpGte | CmpNeq

mkLabels :: [Label] -> Labels
mkLabels = Labels . HashSet.fromList

mkProps :: [(Key, Value)] -> Properties
mkProps = Properties . KeyMap.fromList

idEncoder :: E.Value (Id a)
idEncoder = unId >$< E.int4

nodeEncoder :: E.Value Node
nodeEncoder =
  E.composite
    ( (nodeId >$< E.field (E.nonNullable idEncoder))
        <> (nodeLabels >$< E.field (E.nonNullable labelsEncoder))
        <> (nodeProperties >$< E.field (E.nonNullable propertiesEncoder))
    )

edgeEncoder :: E.Value Edge
edgeEncoder =
  E.composite
    ( (edgeId >$< E.field (E.nonNullable idEncoder))
        <> (edgeLabels >$< E.field (E.nonNullable labelsEncoder))
        <> (edgeProperties >$< E.field (E.nonNullable propertiesEncoder))
        <> (edgeA >$< E.field (E.nonNullable idEncoder))
        <> (edgeB >$< E.field (E.nonNullable idEncoder))
    )

labelsEncoder :: E.Value Labels
labelsEncoder = unLabels >$< E.foldableArray (E.nonNullable E.text)

propertiesEncoder :: E.Value Properties
propertiesEncoder = Object . unProperties >$< E.jsonb

idDecoder :: D.Value (Id a)
idDecoder = Id <$> D.int4

nodeDecoder :: D.Value Node
nodeDecoder =
  D.composite $
    Node
      <$> D.field (D.nonNullable idDecoder)
      <*> D.field (D.nonNullable labelsDecoder)
      <*> D.field (D.nonNullable propertiesDecoder)

edgeDecoder :: D.Value Edge
edgeDecoder =
  D.composite $
    Edge
      <$> D.field (D.nonNullable idDecoder)
      <*> D.field (D.nonNullable labelsDecoder)
      <*> D.field (D.nonNullable propertiesDecoder)
      <*> D.field (D.nonNullable idDecoder)
      <*> D.field (D.nonNullable idDecoder)

labelsDecoder :: D.Value Labels
labelsDecoder = Labels . HashSet.fromList <$> D.listArray (D.nonNullable D.text)

propertiesDecoder :: D.Value Properties
propertiesDecoder = toProperties <$> D.jsonb

instance DefaultParamEncoder Node where
  defaultParam = E.nonNullable nodeEncoder

instance DefaultParamEncoder Edge where
  defaultParam = E.nonNullable edgeEncoder

instance DefaultParamEncoder Labels where
  defaultParam = E.nonNullable labelsEncoder

instance DefaultParamEncoder Properties where
  defaultParam = E.nonNullable propertiesEncoder

instance DefaultParamEncoder (Id a) where
  defaultParam = E.nonNullable idEncoder

toProperties :: Value -> Properties
toProperties (Object o) = Properties o
toProperties _ = error "Only objects accepted"

type Db = Hasql.Session

run :: Db a -> Hasql.Connection -> IO a
run a conn = either (error . show) return =<< Hasql.run a conn

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

mergeNode :: Labels -> Properties -> Db [Id Node]
mergeNode labels props = do
  ns <- matchNode (hasLabels labels .& hasProperties props)
  case ns of
    [] -> do
      n <- createNode labels props
      pure [n]
    xs -> pure xs

mergeEdge :: Labels -> Properties -> Id Node -> Id Node -> Db [Id Edge]
mergeEdge labels props a b = do
  ns <- matchEdge (hasLabels labels .& hasProperties props) (Just a) (Just b)
  case ns of
    [] -> do
      n <- createEdge labels props a b
      pure [n]
    xs -> pure xs