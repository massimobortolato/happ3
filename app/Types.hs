module Types where

import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)

--------------------------------------------------------------------------------
type MaybeConnection = Maybe Connection

--------------------------------------------------------------------------------
newtype AppConfig = AppConfig
  { dbPool :: Pool MaybeConnection
  }

--------------------------------------------------------------------------------
newtype AppM a = AppM {runAppM :: ReaderT AppConfig IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadReader AppConfig)

--------------------------------------------------------------------------------
