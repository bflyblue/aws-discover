{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Database where

import qualified Algebra.Graph.Labelled as G
import Config
import Contravariant.Extras.Contrazip
import Data.Foldable (foldl')
import Data.Functor.Contravariant (contramap, (>$<))
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Vector as Vector
import Database.Types
import qualified Hasql.Connection as Connection
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Session as S
import qualified Hasql.Statement as S

{-
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

nodeEncoder :: E.Params Node
nodeEncoder =
  (unId . nodeId >$< E.param (E.nonNullable E.uuid))
    <> (nodeLabels >$< E.param (E.nonNullable (E.foldableArray (E.nonNullable E.text))))
    <> (nodeProperties >$< E.param (E.nonNullable E.jsonb))

nodeCompositeEncoder :: E.Params Node
nodeCompositeEncoder = E.param (E.nonNullable $ E.composite encoder)
 where
  encoder =
    (unId . nodeId >$< E.field (E.nonNullable E.uuid))
      <> (nodeLabels >$< E.field (E.nonNullable (E.foldableArray (E.nonNullable E.text))))
      <> (nodeProperties >$< E.field (E.nonNullable E.jsonb))

nodesEncoder :: E.Params (Vector.Vector Node)
nodesEncoder =
  (Vector.map (unId . nodeId) >$< vector E.uuid)
    <> (Vector.map nodeLabels >$< vector (E.foldableArray (E.nonNullable E.text)))
    <> (Vector.map nodeProperties >$< vector E.jsonb)

nodesCompositeEncoder :: E.Params (Vector.Vector Node)
nodesCompositeEncoder = vector (E.composite encoder)
 where
  encoder =
    (unId . nodeId >$< E.field (E.nonNullable E.uuid))
      <> (nodeLabels >$< E.field (E.nonNullable (E.foldableArray (E.nonNullable E.text))))
      <> (nodeProperties >$< E.field (E.nonNullable E.jsonb))

edgeEncoder :: E.Params Edge
edgeEncoder =
  (unId . edgeId >$< E.param (E.nonNullable E.uuid))
    <> (edgeLabels >$< E.param (E.nonNullable (E.foldableArray (E.nonNullable E.text))))
    <> (edgeProperties >$< E.param (E.nonNullable E.jsonb))

edgesEncoder :: E.Params (Vector.Vector Edge)
edgesEncoder =
  (Vector.map (unId . edgeId) >$< vector E.uuid)
    <> (Vector.map edgeLabels >$< vector (E.foldableArray (E.nonNullable E.text)))
    <> (Vector.map edgeProperties >$< vector E.jsonb)

edgeMapEncoder :: E.Params (Vector.Vector (Edge, Id Node, Id Node))
edgeMapEncoder =
  contramap Vector.unzip3 $
    contrazip3
      edgesEncoder
      (Vector.map unId >$< vector E.uuid)
      (Vector.map unId >$< vector E.uuid)

edgeMapCompositeEncoder :: E.Params (Vector.Vector (Edge, Id Node, Id Node))
edgeMapCompositeEncoder = vector (E.composite encoder)
 where
  encoder =
    (unId . edgeId . edge >$< E.field (E.nonNullable E.uuid))
      <> (edgeLabels . edge >$< E.field (E.nonNullable (E.foldableArray (E.nonNullable E.text))))
      <> (edgeProperties . edge >$< E.field (E.nonNullable E.jsonb))
      <> (unId . nodeA >$< E.field (E.nonNullable E.uuid))
      <> (unId . nodeB >$< E.field (E.nonNullable E.uuid))
  edge (e, _, _) = e
  nodeA (_, a, _) = a
  nodeB (_, _, b) = b

vector :: E.Value a -> E.Params (Vector.Vector a)
vector = E.param . E.nonNullable . E.array . E.dimension foldl' . E.element . E.nonNullable

nodeDecoder :: D.Row Node
nodeDecoder =
  Node
    <$> (Id <$> D.column (D.nonNullable D.uuid))
    <*> D.column (D.nonNullable (D.listArray (D.nonNullable D.text)))
    <*> D.column (D.nonNullable D.jsonb)

edgeDecoder :: D.Row Edge
edgeDecoder =
  Edge
    <$> (Id <$> D.column (D.nonNullable D.uuid))
    <*> D.column (D.nonNullable (D.listArray (D.nonNullable D.text)))
    <*> D.column (D.nonNullable D.jsonb)

insertGraph :: Graph -> S.Session ()
insertGraph g = do
  S.statement (Vector.fromList $ G.vertexList g) insertNodes
  S.statement (Vector.fromList $ concatMap (\(es, a, b) -> [(e, nodeId a, nodeId b) | e <- es]) $ G.edgeList g) insertEdges

insertNodes :: S.Statement (Vector.Vector Node) ()
insertNodes = S.Statement sql encoder decoder True
 where
  sql = "insert into nodes (id, labels, properties) select * from unnest ($1::nodes[])"
  encoder = nodesCompositeEncoder
  decoder = D.noResult

insertEdges :: S.Statement (Vector.Vector (Edge, Id Node, Id Node)) ()
insertEdges = S.Statement sql encoder decoder True
 where
  sql = "insert into edges (id, labels, properties, a, b) select * from unnest ($1::edges[])"
  encoder = edgeMapCompositeEncoder
  decoder = D.noResult
-}