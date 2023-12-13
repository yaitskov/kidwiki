module Kidwiki.Servant.Monad where

import Control.Monad.Catch (MonadCatch, MonadThrow)
import Katip as K
import Relude
import UnliftIO (MonadUnliftIO)

type UserTableName = Text
type TableFieldName = Text

newtype ServantAppState = ServantAppState { unit :: () }

newtype AppM a
  = AppM
      { runAppM :: ReaderT ServantAppState (K.KatipContextT IO) a
      }
    deriving
      ( Functor
      , Applicative
      , Monad
      , MonadIO
      , MonadFail
      , MonadCatch
      , MonadThrow
      , MonadReader ServantAppState
      , Katip
      , KatipContext
      , MonadUnliftIO
      )
