module Kidwiki.FixedPool where

import Control.Monad.STM (retry)
import Relude (Ord, Eq, ($), (=<<), (>>=), pure, length, Maybe (..), (.))
import UnliftIO
import UnliftIO.Pool

mkFixedPool :: forall a m. (Ord a, Eq a, MonadUnliftIO m) => [a] -> m (Pool a)
mkFixedPool originItems = do
  itemsVar <- newTVarIO originItems
  let
    borrowKey =
      atomically $ do
        readTVar itemsVar >>= \case
          headItem:rest -> do
            writeTVar itemsVar rest
            pure headItem
          [] -> retry
    returnKey a = do
      atomically $ do
        readTVar itemsVar >>= \availaleItems ->
          writeTVar itemsVar (a:availaleItems)
      pure ()

  newPool . setNumStripes (Just 1) =<<
    mkDefaultPoolConfig borrowKey returnKey 10.0 (length originItems)
