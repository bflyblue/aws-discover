module Database where

import Config
import qualified Control.Monad.Trans.Resource as Resource
import Data.Default (def)
import qualified Database.Bolt as Bolt

boltConfig :: Config -> Bolt.BoltCfg
boltConfig cfg =
  def
    { Bolt.host = host cfg
    , Bolt.user = user cfg
    , Bolt.password = password cfg
    , Bolt.secure = secure cfg
    }

withBolt :: Resource.MonadResource m => Bolt.BoltCfg -> (Bolt.Pipe -> m a) -> m a
withBolt cfg f = do
  (key, db) <- Resource.allocate (Bolt.connect cfg) Bolt.close
  r <- f db
  Resource.release key
  return r