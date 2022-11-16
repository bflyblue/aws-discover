{-# LANGUAGE FlexibleInstances #-}

module Tags where

import qualified Amazonka.EC2 as EC2
import qualified Amazonka.RDS as RDS
import qualified Amazonka.ResourceGroupsTagging as ResourceGroupsTagging
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Database.Bolt ((=:))
import qualified Database.Bolt as Bolt

newtype Tags = Tags (Map.Map Text Bolt.Value)

class IsTag a where
  toTags :: a -> Tags

instance IsTag [EC2.Tag] where
  toTags = Tags . Map.fromList . map tagPair
   where
    tagPair (EC2.Tag' k v) = k =: Just v

instance IsTag [RDS.Tag] where
  toTags = Tags . Map.fromList . mapMaybe tagPair
   where
    tagPair (RDS.Tag' (Just k) v) = Just $ k =: Just v
    tagPair (RDS.Tag' Nothing _) = Nothing

instance IsTag [ResourceGroupsTagging.Tag] where
  toTags = Tags . Map.fromList . map tagPair
   where
    tagPair (ResourceGroupsTagging.Tag' k v) = k =: Just v

instance IsTag (HashMap.HashMap Text Text) where
  toTags = Tags . Map.fromList . HashMap.toList . fmap Bolt.toValue

instance Bolt.IsValue Tags where
  toValue (Tags tags) = Bolt.toValue tags
