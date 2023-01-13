{-# LANGUAGE OverloadedStrings #-}

module Database.Match where

import Database.Types

import qualified Data.Aeson.Types as Aeson
import Data.Text (Text)
import GHC.Exts (fromList)
import qualified Hasql.DynamicStatements.Snippet as Snippet

data MatchExpr
  = HasLabels Labels
  | HasProperties Properties
  | PropCmp Text Cmp Aeson.Value
  | PropIn Text In [Aeson.Value]
  | PropElem Aeson.Value In Text
  | MatchAnd MatchExpr MatchExpr
  | MatchOr MatchExpr MatchExpr
  | MatchNot MatchExpr

data Cmp = Eq | Lt | Lte | Gt | Gte | Neq

data In = In | NotIn

hasLabels :: Labels -> MatchExpr
hasLabels = HasLabels

hasLabel :: Label -> MatchExpr
hasLabel a = HasLabels $ fromList [a]

hasProperties :: Properties -> MatchExpr
hasProperties = HasProperties

propertyIn :: Text -> [Aeson.Value] -> MatchExpr
propertyIn f = PropIn f In

propertyNotIn :: Text -> [Aeson.Value] -> MatchExpr
propertyNotIn f = PropIn f NotIn

propertyElemOf :: Aeson.Value -> Text -> MatchExpr
propertyElemOf v = PropElem v In

propertyNotElemOf :: Aeson.Value -> Text -> MatchExpr
propertyNotElemOf v = PropElem v NotIn

(.=.), (.!=.), (.<.), (.<=.), (.>.), (.>=.) :: Text -> Aeson.Value -> MatchExpr
f .=. v = PropCmp f Eq v
f .!=. v = PropCmp f Neq v
f .<. v = PropCmp f Lt v
f .<=. v = PropCmp f Lte v
f .>. v = PropCmp f Gt v
f .>=. v = PropCmp f Gte v

infix 4 .=., .!=., .<., .<=., .>., .>=.

(.&.), (.|.) :: MatchExpr -> MatchExpr -> MatchExpr
(.&.) = MatchAnd
(.|.) = MatchOr

infixl 3 .&.
infixl 2 .|.

not :: MatchExpr -> MatchExpr
not = MatchNot

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