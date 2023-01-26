{-# OPTIONS_GHC -Wno-orphans #-}

module AWS.ResourceGroupsTagging.Orphans where

import Data.Aeson

import qualified Amazonka.ResourceGroupsTagging.Types.Tag as Tag

instance ToJSON Tag.Tag where
  toEncoding = genericToEncoding defaultOptions
