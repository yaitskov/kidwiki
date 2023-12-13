{-# LANGUAGE DeriveGeneric #-}
module Kidwiki.Wikipedia.Api where

import Control.Monad.Catch
import Kidwiki.Api
import Kidwiki.Servant.Monad
import Data.Aeson as A
import Data.Text qualified as T
import Network.HTTP.Req as R
import Relude
import Servant as S

newtype WikipediaPage
  = WikipediaPage
    { htmlContent :: Text
    } deriving (Show, Eq, Generic, FromJSON, ToJSON)

type DownloadPage =
  "download" :>
  S.QueryParams "path" Text :>
  S.Get '[JSON] WikipediaPage

type WikipediaApi
  = "wikipedia" :>
      DownloadPage

type DownloadPageFun = [Text] -> AppM WikipediaPage

wikipedia ::
  DownloadPageFun
wikipedia =
  downloadPage

textToPath :: forall a. Url a -> Text -> Url a
textToPath b = foldl' (/~) b . T.split ('/' ==)

downloadPage :: DownloadPageFun
downloadPage wps =
  head400 "no path parameter" wps $ \wp -> do
    res :: BsResponse <- runReq defaultHttpConfig $ R.req R.GET
      (textToPath (https "en.wikipedia.org") wp)
      NoReqBody bsResponse mempty --
    case responseStatusCode res of
        200 ->
          pure . WikipediaPage . decodeUtf8 $ R.responseBody res
        badStatus ->
          throwM err400
            { errBody = "Wikipedia responded with " <> show badStatus }
