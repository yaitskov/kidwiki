module Kidwiki.Types where

import Control.Monad.Reader
import Network.HTTP.Client
import Data.Aeson
import Data.Data
import Data.Time.LocalTime
import Relude

newtype AppSt
  = AppSt
    { manager :: Manager
    }

type AppT = ReaderT AppSt IO

-- ChartId don't need to store MetricId because it is known from context
-- and const for all Charts in a Rule because Rule depends on 1 metric
type ChartId = LocalTime

type PointIdxInPeriod = Int32

type IndexedChartPoints = Map PointIdxInPeriod Double

localTimeToDouble :: LocalTime -> Double
localTimeToDouble (LocalTime days timeOfDay) =
  fromIntegral (daysAsSec + timeAsSec)
  where
    daysAsSec = fromEnum days * 3600 * 24
    (seconds, _lessThanSecond) = properFraction $ todSec timeOfDay
    timeAsSec = todHour timeOfDay * 3600 + todMin timeOfDay * 60 + seconds

newtype AppName = AppName {unAppName :: Text}
  deriving (Eq, Show, Ord, Data, Generic)
  deriving newtype (FromJSON, ToJSON, Semigroup)

newtype HttpAppPort = HttpAppPort Int deriving newtype (Eq, Show)
newtype FrontEndPort = FrontEndPort Int deriving newtype (Eq, Show)
