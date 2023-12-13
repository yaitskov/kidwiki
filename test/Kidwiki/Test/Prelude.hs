{-# OPTIONS_HADDOCK hide #-}

module CoMajor.Test.Prelude (module X) where

import CoMajor.Prelude as X hiding (collect)
-- import System.Directory as X (doesFileExist, removeFile)
import Test.QuickCheck.Instances as X ()
import Test.Tasty as X
import Test.Tasty.Golden as X
import Test.Tasty.HUnit as X
import Test.Tasty.QuickCheck as X hiding (Failure, Success, tables, (.&&.))
import UnliftIO as X (withSystemTempDirectory, withSystemTempFile)
