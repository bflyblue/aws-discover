{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Types where

import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as Aeson
import Data.ByteString (ByteString)
import Data.Functor.Contravariant ((>$<))
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Int (Int32)
import Data.Text (Text)
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import Hasql.Implicits.Encoders (DefaultParamEncoder (..))
import qualified Hasql.Session as Hasql

type Db = Hasql.Session

type Label = Text

newtype Labels = Labels {unLabels :: HashSet Label}
  deriving (Show, Eq, Ord, Semigroup, Monoid)

newtype Properties = Properties {unProperties :: Aeson.Object}
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

instance Hasql.Implicits.Encoders.DefaultParamEncoder Node where
  defaultParam = E.nonNullable nodeEncoder

instance Hasql.Implicits.Encoders.DefaultParamEncoder Edge where
  defaultParam = E.nonNullable edgeEncoder

instance Hasql.Implicits.Encoders.DefaultParamEncoder Labels where
  defaultParam = E.nonNullable labelsEncoder

instance Hasql.Implicits.Encoders.DefaultParamEncoder Properties where
  defaultParam = E.nonNullable propertiesEncoder

instance Hasql.Implicits.Encoders.DefaultParamEncoder (Id a) where
  defaultParam = E.nonNullable idEncoder

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
propertiesEncoder = Aeson.Object . unProperties >$< E.jsonb

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

toProperties :: Aeson.Value -> Properties
toProperties (Aeson.Object o) = Properties o
toProperties _ = error "Only objects accepted"

mkLabels :: [Label] -> Labels
mkLabels = Labels . HashSet.fromList

mkProps :: [(Aeson.Key, Aeson.Value)] -> Properties
mkProps = Properties . KeyMap.fromList
