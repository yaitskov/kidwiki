module Kidwiki.Servant.Monad where

import Control.Monad.Catch (MonadCatch, MonadThrow)
import Katip as K
import Relude
import UnliftIO (MonadUnliftIO)
import UnliftIO.Pool

newtype GptKey = GptKey ByteString deriving (Show, Eq, Ord)

data ServantAppState
  = ServantAppState
      { unit :: ()
      , gptKeysPool :: Pool GptKey
      }

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
