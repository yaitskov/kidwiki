module Kidwiki.Error where

import Relude

failLeft :: MonadFail m => Either String a -> m a
failLeft = \case
  Left e -> fail e
  Right r -> pure r
