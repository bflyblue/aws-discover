{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module AWS.ResourceGroupsTagging.Resources where

import AWS.ResourceGroupsTagging.Orphans ()
import qualified Amazonka
import qualified Amazonka.ResourceGroupsTagging as ResourceGroupsTagging
import qualified Amazonka.ResourceGroupsTagging.GetResources as GetResources
import qualified Amazonka.ResourceGroupsTagging.Types.ResourceTagMapping as ResourceTagMapping
import Conduit
import Config
import Data.Aeson ((.=))
import Data.Foldable (traverse_)
import Database

fetchAllResourceTagMappings :: MonadResource m => Amazonka.Env -> ConduitM () ResourceGroupsTagging.ResourceTagMapping m ()
fetchAllResourceTagMappings env =
  Amazonka.paginate env ResourceGroupsTagging.newGetResources
    .| concatMapC (concat . GetResources.resourceTagMappingList)

ingestResourceTags :: MonadIO m => Pool -> ConduitT ResourceGroupsTagging.ResourceTagMapping o m ()
ingestResourceTags pool = mapM_C ingestTags
 where
  ingestTags :: MonadIO m => ResourceGroupsTagging.ResourceTagMapping -> m ()
  ingestTags mapping = liftIO $
    run pool $ do
      traverse_
        ( \arn -> do
            upsertNode
              ("arn", arn)
              ["Resource"]
              ( maybe
                  mempty
                  ( \tags ->
                      properties
                        [ "resourceTags" .= tags
                        ]
                  )
                  (ResourceTagMapping.tags mapping)
              )
        )
        (ResourceTagMapping.resourceARN mapping)

discover :: Amazonka.Env -> Config -> IO ()
discover env cfg =
  withDb cfg $ \pool ->
    runResourceT $ runConduit $ fetchAllResourceTagMappings env .| ingestResourceTags pool