{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Match where

import Database.Types

import qualified Data.Aeson.Types as Aeson
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import GHC.Exts (fromList)
import qualified Hasql.DynamicStatements.Snippet as Snippet
import Hasql.Implicits.Encoders (DefaultParamEncoder)

data Cmp = Eq | Neq | Lt | Lte | Gt | Gte
  deriving (Show, Eq, Ord)

data Path a where
  PCtx :: Path Aeson.Object
  PKey :: Aeson.FromJSON a => Path Aeson.Object -> Text -> Path a

data Match a where
  MLit :: DefaultParamEncoder a => a -> Match a
  MProps :: Match Aeson.Object
  MLabels :: Match Labels
  MHasProperties :: Properties -> Match Bool
  MHasLabels :: Labels -> Match Bool
  MPath :: Match a -> Path b -> Match [b]
  MCmp :: Match a -> Cmp -> Match a -> Match Bool
  MMember :: Match a -> Match [a] -> Match Bool
  MAnd :: Match Bool -> Match Bool -> Match Bool
  MOr :: Match Bool -> Match Bool -> Match Bool
  MNot :: Match Bool -> Match Bool

(.=.), (.!=.), (.<.), (.<=.), (.>.), (.>=.) :: Match a -> Match a -> Match Bool
a .=. b = MCmp a Eq b
a .!=. b = MCmp a Neq b
a .<. b = MCmp a Lt b
a .<=. b = MCmp a Lte b
a .>. b = MCmp a Gt b
a .>=. b = MCmp a Gte b

infix 4 .=., .!=., .<., .<=., .>., .>=.

(.&.), (.|.) :: Match Bool -> Match Bool -> Match Bool
(.&.) = MAnd
(.|.) = MOr

infixl 3 .&.
infixl 2 .|.

in_ :: Match a -> Match [a] -> Match Bool
in_ = MMember

not :: Match Bool -> Match Bool
not = MNot

lit :: DefaultParamEncoder a => a -> Match a
lit = MLit

txt :: Text -> Match Text
txt = lit

props :: Path b -> Match [b]
props = MPath MProps

ctx :: Path Aeson.Object
ctx = PCtx

(.-) :: Aeson.FromJSON a => Path Aeson.Object -> Text -> Path a
o .- k = PKey o k

infixl 9 .-

cmp :: Cmp -> Snippet.Snippet
cmp Eq = "=="
cmp Neq = "!="
cmp Lt = "<"
cmp Lte = "<="
cmp Gt = ">"
cmp Gte = ">="

match :: Match a -> Snippet.Snippet
match (MLit a) = Snippet.param a
match MProps = "properties"
match MLabels = "labels"
match (MHasProperties props') = "(properties @> " <> Snippet.param props' <> ")"
match (MHasLabels labels) = "(labels @> " <> Snippet.param labels <> ")"
match (MPath a b) = "(jsonb_path_query_array(" <> match a <> ",'" <> path b <> "'))"
match (MCmp a op b) = "(" <> match a <> cmp op <> match b <> ")"
match (MMember a b) = "(" <> match b <> "@>" <> match a <> ")"
match (MAnd a b) = "(" <> match a <> " and " <> match b <> ")"
match (MOr a b) = "(" <> match a <> " or " <> match b <> ")"
match (MNot a) = "not(" <> match a <> ")"

path :: Path a -> Snippet.Snippet
path PCtx = "$"
path (PKey a b) = path a <> "." <> sqlText b

sqlText :: Text -> Snippet.Snippet
sqlText = Snippet.sql . encodeUtf8

hasLabels :: Labels -> Match Bool
hasLabels = MHasLabels

hasLabel :: Label -> Match Bool
hasLabel a = MHasLabels $ fromList [a]

hasProperties :: Properties -> Match Bool
hasProperties = MHasProperties

{-
data In = In | NotIn

propertyIn :: Text -> [Aeson.Value] -> MatchExpr
propertyIn f = PropIn f In

propertyNotIn :: Text -> [Aeson.Value] -> MatchExpr
propertyNotIn f = PropIn f NotIn

propertyElemOf :: Aeson.Value -> Text -> MatchExpr
propertyElemOf v = PropElem v In

propertyNotElemOf :: Aeson.Value -> Text -> MatchExpr
propertyNotElemOf v = PropElem v NotIn

matchExpr :: MatchExpr -> Snippet.Snippet
matchExpr (HasLabels labels) = "(labels @> " <> Snippet.param labels <> ")"
matchExpr (HasProperties props) = "(properties @> " <> Snippet.param props <> ")"
matchExpr (PropCmp field op val) = "(properties->" <> Snippet.param field <> cmpExpr op <> Snippet.param val <> ")"
matchExpr (PropIn field In vals) = "(properties->" <> Snippet.param field <> "= any(" <> Snippet.param vals <> "))"
matchExpr (PropIn field NotIn vals) = "(properties->" <> Snippet.param field <> "!= all(" <> Snippet.param vals <> ")"
matchExpr (PropElem val In field) = "(properties->" <> Snippet.param field <> " @> " <> Snippet.param val <> ")"
matchExpr (PropElem val NotIn field) = "not(properties->" <> Snippet.param field <> " @> " <> Snippet.param val <> ")"
matchExpr (MatchAnd a b) = "(" <> matchExpr a <> " and " <> matchExpr b <> ")"
matchExpr (MatchOr a b) = "(" <> matchExpr a <> " or " <> matchExpr b <> ")"
matchExpr (MatchNot a) = "not(" <> matchExpr a <> ")"

cmpExpr :: Cmp -> Snippet.Snippet
cmpExpr Eq = "="
cmpExpr Lt = "<"
cmpExpr Lte = "<="
cmpExpr Gt = ">"
cmpExpr Gte = ">="
cmpExpr Neq = "!="
-}