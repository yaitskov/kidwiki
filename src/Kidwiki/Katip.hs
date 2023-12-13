module Kidwiki.Katip where

import Katip
import Relude
import UnliftIO as UIO

withKatipToStdout ::
  forall m a. MonadUnliftIO m =>
  Namespace ->
  Environment ->
  KatipContextT m a ->
  (LogEnv -> m ()) ->
  m a
withKatipToStdout ns env inKatipCtxAction afterKatipCtxAction = do
  handleScribe <- liftIO $ mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
  let makeLogEnv =
        registerScribe "stdout" handleScribe defaultScribeSettings
          =<< initLogEnv ns env
  UIO.bracket (liftIO makeLogEnv) (liftIO . closeScribes) $
    \le -> do
      a <- runKatipContextT le () ns inKatipCtxAction
      afterKatipCtxAction le
      pure a
