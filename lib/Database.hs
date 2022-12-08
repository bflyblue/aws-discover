{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Database where

import Config
import Control.Monad.Reader
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types
import Data.Functor.Contravariant ((>$), (>$<))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Int
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Hasql.Connection as Connection
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import Hasql.Implicits.Encoders
import qualified Hasql.Session as Hasql
import qualified Hasql.Statement as Hasql

withDb :: Config -> (Connection.Connection -> IO a) -> IO a
withDb cfg f = do
  let PostgresConfig{host, port, user, password, name} = database cfg
  connection <-
    Connection.acquire
      ( Connection.settings
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
      Connection.release con
      return r

type Id = Int32

type Label = Text

newtype Labels = Labels {unLabels :: HashSet Label}
  deriving (Show, Eq, Ord, Semigroup, Monoid)

newtype Properties = Properties {unProperties :: Object}
  deriving (Show, Eq, Ord, Semigroup, Monoid)

data Node = Node
  { nodeId :: Id
  , nodeLabels :: Labels
  , nodeProperties :: Properties
  }
  deriving (Show)

data Edge = Edge
  { edgeId :: Id
  , edgeLabels :: Labels
  , edgeProperties :: Properties
  , edgeA :: Id
  , edgeB :: Id
  }
  deriving (Show)

nodeEncoder :: E.Value Node
nodeEncoder =
  E.composite
    ( (nodeId >$< E.field (E.nonNullable E.int4))
        <> (nodeLabels >$< E.field (E.nonNullable labelsEncoder))
        <> (nodeProperties >$< E.field (E.nonNullable propertiesEncoder))
    )

edgeEncoder :: E.Value Edge
edgeEncoder =
  E.composite
    ( (edgeId >$< E.field (E.nonNullable E.int4))
        <> (edgeLabels >$< E.field (E.nonNullable labelsEncoder))
        <> (edgeProperties >$< E.field (E.nonNullable propertiesEncoder))
        <> (edgeA >$< E.field (E.nonNullable E.int4))
        <> (edgeB >$< E.field (E.nonNullable E.int4))
    )

labelsEncoder :: E.Value Labels
labelsEncoder = unLabels >$< E.foldableArray (E.nonNullable E.text)

propertiesEncoder :: E.Value Properties
propertiesEncoder = Object . unProperties >$< E.jsonb

nodeDecoder :: D.Value Node
nodeDecoder =
  D.composite $
    Node
      <$> D.field (D.nonNullable D.int4)
      <*> D.field (D.nonNullable labelsDecoder)
      <*> D.field (D.nonNullable propertiesDecoder)

edgeDecoder :: D.Value Edge
edgeDecoder =
  D.composite $
    Edge
      <$> D.field (D.nonNullable D.int4)
      <*> D.field (D.nonNullable labelsDecoder)
      <*> D.field (D.nonNullable propertiesDecoder)
      <*> D.field (D.nonNullable D.int4)
      <*> D.field (D.nonNullable D.int4)

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

toProperties :: Value -> Properties
toProperties (Object o) = Properties o
toProperties _ = error "Only objects accepted"

type VarName = Text

data Expr a where
  Lit :: DefaultParamEncoder a => a -> Expr a
  Let :: Let b => VarName -> Expr b -> Expr a -> Expr a
  NewNode :: Expr Labels -> Expr Properties -> Expr Node
  AddNodeLabels :: Expr Labels -> VarName -> Expr ()

-- MergeNodeProperties :: Expr Properties -> Expr Id -> Expr ()
-- MatchNode :: Labels -> Properties -> Expr Id

lit :: DefaultParamEncoder a => a -> Expr a
lit = Lit

mkLabels :: [Label] -> Labels
mkLabels = Labels . HashSet.fromList

mkProps :: [(Key, Value)] -> Properties
mkProps = Properties . KeyMap.fromList

data Var = VNode Node | VEdge Edge

type E = ReaderT (HashMap Text Var) Hasql.Session

runE :: E a -> Hasql.Session a
runE a = runReaderT a HashMap.empty

runExpr :: Expr a -> Hasql.Session a
runExpr = runE . go

go :: Expr a -> E a
go (Lit a) = pure a
go (Let name val expr) = letin name val expr
go (NewNode labels props) = do
  l <- go labels
  p <- go props
  newNode l p
go (AddNodeLabels labels var) = do
  l <- go labels
  vars <- ask
  case HashMap.lookup var vars of
    Just (VNode node) -> addNodeLabels l (nodeId node)
    Just _ -> error "expecting variable of type Node"
    Nothing -> error "missing variable"

class Let a where
  letin :: Text -> Expr a -> Expr b -> E b

instance Let Node where
  letin name val expr = do
    v <- go val
    local (HashMap.insert name (VNode v)) (go expr)

instance Let Edge where
  letin name val expr = do
    v <- go val
    local (HashMap.insert name (VEdge v)) (go expr)

newNode :: Labels -> Properties -> E Node
newNode labels props = lift $ Hasql.statement () s
 where
  s = Hasql.Statement sql encoder decoder True
  sql = "insert into nodes (labels, properties) values ($1, $2) returning nodes"
  encoder =
    (labels >$ E.param (E.nonNullable labelsEncoder))
      <> (props >$ E.param (E.nonNullable propertiesEncoder))
  decoder =
    D.singleRow (D.column (D.nonNullable nodeDecoder))

addNodeLabels :: Labels -> Id -> E ()
addNodeLabels labels nodeid = lift $ Hasql.statement () s
 where
  s = Hasql.Statement sql encoder decoder True
  sql = "update nodes set labels = array(select distinct unnest(labels || $1)) where id = $2"
  encoder =
    (labels >$ E.param (E.nonNullable labelsEncoder))
      <> (nodeid >$ E.param (E.nonNullable E.int4))
  decoder = D.noResult