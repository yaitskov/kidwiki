module Kidwiki.Gpt.Api where

import Control.Lens
import Control.Monad.Catch
import Kidwiki.Servant.Monad
import Data.Aeson as A
import Data.Generics.Labels ()
import Network.HTTP.Req as R
import Relude
import Servant as S
import UnliftIO.Pool

gptAesonOptions :: Options
gptAesonOptions = A.defaultOptions { fieldLabelModifier = camelTo2 '_' }

data GptResponse
  = GptResponse
      { id :: Text
      , object :: Text
      , created :: Integer
      , model :: Text
      , choices :: [GptChoice]
      -- , usage :: GptResponseUsage
      -- , system_fingerprint :: Maybe Text
      } deriving (Show, Eq, Generic)

instance ToJSON GptResponse where
  toJSON = genericToJSON gptAesonOptions
instance FromJSON GptResponse where
  parseJSON = genericParseJSON gptAesonOptions

data GptChoice
  = GptChoice
      { index :: Int
      , message :: GptMessage
      , finishReason :: FinishReason
      } deriving (Show, Eq, Generic)

instance ToJSON GptChoice where
  toJSON = genericToJSON gptAesonOptions
instance FromJSON GptChoice where
  parseJSON = genericParseJSON gptAesonOptions

data GptMessage
  = GptMessage
      { role :: GptRole
      , content :: Text
      } deriving (Show, Eq, Generic)

instance ToJSON GptMessage where
  toJSON = genericToJSON gptAesonOptions
instance FromJSON GptMessage where
  parseJSON = genericParseJSON gptAesonOptions

type GptRole = Text
type FinishReason = Text

newtype GptModel = GptModel Text deriving (Show, Eq, Generic, FromJSON, ToJSON)

gpt3AndHalfModel :: GptModel
gpt3AndHalfModel = GptModel "gpt-3.5-turbo"

data GptRequest
  = GptRequest
      { model :: GptModel
      , messages :: [GptMessage]
      } deriving (Show, Eq, Generic)

instance ToJSON GptRequest where
  toJSON = genericToJSON gptAesonOptions
instance FromJSON GptRequest where
  parseJSON = genericParseJSON gptAesonOptions

newtype TextForRephrase
  = TextForRephrase Text
  deriving (Show, Eq, FromHttpApiData, FromJSON, ToJSON)
newtype RephraseGoal
  = RephraseGoal Text
  deriving (Show, Eq, FromHttpApiData, FromJSON, ToJSON)
newtype RephrasedText
  = RephrasedText Text
  deriving (Show, Eq, FromHttpApiData, FromJSON, ToJSON)

type RephraseTextFor =
  "rephrase" :>
  S.QueryParam' '[Required, Strict] "text" TextForRephrase :>
  S.QueryParam' '[Required, Strict] "goal" RephraseGoal :>
  S.Get '[JSON] RephrasedText

type GptApi
  = "gpt" :>
      RephraseTextFor

type RephraseTextForFun =
  TextForRephrase -> RephraseGoal -> AppM RephrasedText

gpt ::
  RephraseTextForFun
gpt =
  rephraseTextFor

rephraseTextFor :: RephraseTextForFun
rephraseTextFor (TextForRephrase txt) (RephraseGoal goal) = do
  gkp <- asks gptKeysPool
  withResource gkp go
  where
    url =
      https "api.openai.com" /~
        ("v1" :: Text) /~
          ("chat" :: Text) /~
            ("completions" :: Text)
    sysRole =
      GptMessage
        { role = "system"
        , content =
            "Summarize content you are provided with for a " <> goal <> "."
        -- second-grade student.
        }

    userRole =
      GptMessage
        { role = "user"
        , content = txt
        }

    gptReq = GptRequest gpt3AndHalfModel [sysRole, userRole]

    go (GptKey gptKey) = do
      res :: JsonResponse GptResponse <- runReq defaultHttpConfig $
        R.req R.POST url (ReqBodyJson gptReq) jsonResponse headers
      case responseStatusCode res of
          200 ->
            let gptRes = R.responseBody res in
              case choices gptRes of
                gptChoice : _ ->
                  pure . RephrasedText $ gptChoice ^. #message . #content
                _ -> throwM err500 { errBody = "GPT returned an empty list" }
          badStatus ->
            throwM err400
              { errBody = "Wikipedia responded with: " <> show badStatus }
      where
        headers =
          R.header "Content-Type" "application/json" <>
            R.header "Authorization" ("Bearer " <> gptKey)
