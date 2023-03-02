{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Match where

import Database.Types

import qualified Data.Aeson.Types as Aeson
import qualified Data.HashSet as HS
import Data.Int
import Data.String (IsString)
import Data.Text (Text)
import qualified Hasql.DynamicStatements.Snippet as Snippet
import Hasql.Implicits.Encoders (DefaultParamEncoder)

class Contains a b

instance {-# OVERLAPPABLE #-} Contains a a
instance Contains [a] a
instance Contains [a] [a]
instance Contains Aeson.Value Aeson.Value

data Match a where
  MLit :: DefaultParamEncoder a => a -> Match a
  MCast :: Match b -> Match a
  MProps :: Match Aeson.Value
  MLabels :: Match [Label]
  MIndex :: Match Aeson.Value -> Int32 -> Match Aeson.Value
  MField :: Match Aeson.Value -> Text -> Match Aeson.Value
  MSub :: Match Aeson.Value -> [Text] -> Match Aeson.Value
  MCmp :: Match a -> Cmp -> Match a -> Match Bool
  MContains :: Contains a b => Match a -> Match b -> Match Bool
  MContainedBy :: Contains b a => Match a -> Match b -> Match Bool
  MAnd :: Match Bool -> Match Bool -> Match Bool
  MOr :: Match Bool -> Match Bool -> Match Bool
  MNot :: Match Bool -> Match Bool

data Cmp = Eq | Neq | Lt | Lte | Gt | Gte
  deriving (Show, Eq, Ord)

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

not :: Match Bool -> Match Bool
not = MNot

lit :: DefaultParamEncoder a => a -> Match a
lit = MLit

cast :: Match b -> Match a
cast = MCast

(.-) :: Match Aeson.Value -> Text -> Match Aeson.Value
o .- k = MField o k

infixl 9 .-

(.@>.) :: Contains a b => Match a -> Match b -> Match Bool
a .@>. b = MContains a b

(.<@.) :: Contains b a => Match a -> Match b -> Match Bool
a .<@. b = MContainedBy a b

infix 6 .@>.
infix 6 .<@.

cmp :: Cmp -> Snippet.Snippet
cmp Eq = "="
cmp Neq = "!="
cmp Lt = "<"
cmp Lte = "<="
cmp Gt = ">"
cmp Gte = ">="

match :: Match a -> Snippet.Snippet
match (MLit a) = Snippet.param a
match (MCast a) = match a
match MProps = "properties"
match MLabels = "labels"
match (MField a f) = bracket $ match a <> "->" <> Snippet.param f
match (MIndex a i) = bracket $ match a <> "->" <> Snippet.param i
match (MSub a fs) = bracket $ match a <> "->" <> Snippet.param fs
match (MCmp a op b) = bracket $ match a <> cmp op <> match b
match (MContains a b) = bracket $ match a <> "@>" <> match b
match (MContainedBy a b) = bracket $ match a <> "<@" <> match b
match (MAnd a b) = bracket $ match a <> " and " <> match b
match (MOr a b) = bracket $ match a <> " or " <> match b
match (MNot a) = "not " <> match a

bracket :: (Semigroup a, IsString a) => a -> a
bracket x = "(" <> x <> ")"

hasLabels :: Labels -> Match Bool
hasLabels (Labels ls) = MLabels .@>. MLit (HS.toList ls)

hasLabel :: Label -> Match Bool
hasLabel = hasLabels . Labels . HS.singleton

hasProperties :: Properties -> Match Bool
hasProperties ps = MProps .@>. MLit (Aeson.Object $ unProperties ps)

labels :: Match [Label]
labels = MLabels

props :: Match Aeson.Value
props = MProps