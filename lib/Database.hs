{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Database where

import Config

-- import Control.Monad.State.Strict
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types
import Data.Functor.Contravariant ((>$), (>$<))

-- import Data.HashMap.Strict (HashMap)
-- import qualified Data.HashMap.Strict as HashMap

import Data.ByteString (ByteString)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Int
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Hasql.Connection as Hasql
import qualified Hasql.Decoders as D
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
addLabels labels nodes = Hasql.statement () statement
 where
  statement = Hasql.Statement sql encoder decoder True
  sql = "update " <> tableName @a <> " set labels = array(select distinct unnest(labels || $1)) where id = any($2)"
  encoder =
    (labels >$ E.param (E.nonNullable labelsEncoder))
      <> (nodes >$ E.param (E.nonNullable (E.foldableArray (E.nonNullable idEncoder))))
  decoder = D.noResult

addProperties :: forall a. HasTable a => Properties -> [Id a] -> Db ()
addProperties props nodes = Hasql.statement () statement
 where
  statement = Hasql.Statement sql encoder decoder True
  sql = "update " <> tableName @a <> " set properties = properties || $1 where id = any($2)"
  encoder =
    (props >$ E.param (E.nonNullable propertiesEncoder))
      <> (nodes >$ E.param (E.nonNullable (E.foldableArray (E.nonNullable idEncoder))))
  decoder = D.noResult