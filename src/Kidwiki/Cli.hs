module Kidwiki.Cli where

import Kidwiki.Cmd.RunServer
import Kidwiki.Lib
import Kidwiki.Types
import Control.Monad.Reader
import Options.Applicative
import Relude

newtype KidwikiCommand = RunServer HttpAppPort deriving (Show, Eq)

backEndPortOption :: Parser HttpAppPort
backEndPortOption = HttpAppPort <$>
  option
    auto (short 'p'
            <> long "port"
            <> value 8182
            <> showDefault
            <> help "HTTP backend port to listen to")


cmdArgsParser :: Parser KidwikiCommand
cmdArgsParser = RunServer <$> backEndPortOption

parseCmdArgs :: IO KidwikiCommand
parseCmdArgs =
  execParser
    (info
      (cmdArgsParser <**> helper)
      (progDesc "Kidwiki")
    )

runKidwikiCommand :: KidwikiCommand -> IO ()
runKidwikiCommand = \case
  RunServer  backPort ->
    mkAppSt >>=
      runReaderT (runServerCmd backPort)
