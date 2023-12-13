module Driver where

import qualified Autodiscover
import Kidwiki.Test.Prelude
import qualified Test.Tasty.Runners.Reporter as Reporter

main :: IO ()
main = do
  onCI <- isJust <$> lookupEnv "CI_JOB_TOKEN"
  defaultMainWithIngredients (if onCI then [Reporter.ingredient] else defaultIngredients) =<< kidwikiTestTree

kidwikiTestTree :: IO TestTree
kidwikiTestTree = do
  autodiscoveredTests <- Autodiscover.tests
  pure $
    testGroup
      "Kidwiki"
      [ autodiscoveredTests
      ]
