module Kidwiki.Time
  ( module Kidwiki.Time
  , module Data.Time.LocalTime
  ) where

import Data.Time.Clock
import Data.Time.LocalTime
import Prelude qualified as P
import Relude

localTimeNow :: forall m. (MonadIO m) => m LocalTime
localTimeNow =
  utcToLocalTime (P.read @TimeZone "UTC") <$> liftIO getCurrentTime
