module Kidwiki.Api where

import Control.Monad.Catch
import Relude
import Servant

nothing404 :: MonadThrow m => LByteString -> Maybe a -> m a
nothing404 _ (Just v) = pure v
nothing404 errorMsg Nothing = throwM err404 { errBody = errorMsg }

memptyTo404 ::
  (Monoid a, Eq a, MonadThrow m) =>
  LByteString ->
  a ->
  (a -> m b) ->
  m b
memptyTo404 errorMsg a f
  | mempty == a = throwM err404 { errBody = errorMsg }
  | otherwise = f a

head400 ::
  (Monoid a, Eq a, MonadThrow m) =>
  LByteString ->
  [a] ->
  (a -> m b) ->
  m b
head400 _ (a:_) f = f a
head400 errorMsg [] _ = throwM err400 { errBody = errorMsg }

left400 :: MonadThrow m => Either LByteString a -> m a
left400 (Right v) = pure v
left400 (Left errorMsg) = throwM err400 { errBody = errorMsg }

assert400 :: MonadThrow m => LByteString -> Bool -> m a -> m a
assert400 e c a
  | c = a
  | otherwise = throwM err400 { errBody = e }
