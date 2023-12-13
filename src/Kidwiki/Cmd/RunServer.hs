module Kidwiki.Cmd.RunServer where

import Kidwiki.FixedPool
import Kidwiki.Gpt.Api
import Kidwiki.Katip
import Kidwiki.Prelude
import Kidwiki.Servant.Monad
import Kidwiki.Wikipedia.Api
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Logger
import Servant
import System.Posix.Signals hiding (Handler)
import UnliftIO.Directory

type IamReady = "i-am-ready" :> Post '[JSON] Text

type API =
  WikipediaApi :<|>
  GptApi :<|>
  IamReady

api :: Proxy API
api = Proxy

server :: ServerT API AppM
server = wikipedia :<|> gpt :<|> iamReady
  where
    iamReady = pure "Kidwiki Backend"

hoistAppServer ::
  LogEnv -> ServantAppState -> Server API
hoistAppServer le config = hoistServer api (nt' config) server where
  nt' ::
    ServantAppState ->
    AppM a ->
    Handler a
  nt' env m =
    Handler $
      ExceptT $
        try (
          runKatipContextT le () "servant.http" $
            runReaderT (runAppM m) env)


runServerCmd :: HttpAppPort -> AppT ()
runServerCmd httpPort =
  liftIO $ do
    keysPool <- mkFixedPool =<< loadKeys
    withKatipToStdout "Kidwiki" "devel" (pure ()) $ \le ->
      withStdoutLogger
        (\logger ->
           runSettings
              (settings le logger)
              (serve api $
                hoistAppServer le $
                  ServantAppState () keysPool))
  where
    loadKeys =
      fmap GptKey <$>
        (mapM (readFileBS . ("kidwiki.keys/" <>))
          =<< listDirectory "kidwiki.keys")
    whenReady le =
      runKatipContextT le () "servant.init" $ do
        $(logTM) InfoS $ "Listen to port " <> showLS httpPort
    onExit closeSocket =
      forM_ [sigTERM, sigINT, sigQUIT] $ \sig ->
        void $
          installHandler
            sig
            (Catch closeSocket)
            Nothing

    settings :: LogEnv ->
      (Request -> Status -> Maybe Integer -> IO ()) ->
      Settings
    settings le l =
      setBeforeMainLoop (whenReady le)
        $ setServerName "Kidwiki"
        $ setInstallShutdownHandler onExit
        $ setLogger l
        $ setPort (coerce httpPort) defaultSettings
