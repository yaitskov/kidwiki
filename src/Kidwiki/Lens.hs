module Kidwiki.Lens where

import Relude

setDefault :: v -> Maybe v -> Maybe v
setDefault dv =
  \case
    Nothing -> Just dv
    Just v -> Just v
