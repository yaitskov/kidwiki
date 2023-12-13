module Main where

import Kidwiki.Cli qualified as Cli

import Relude

main :: IO ()
main = Cli.parseCmdArgs >>= Cli.runKidwikiCommand
