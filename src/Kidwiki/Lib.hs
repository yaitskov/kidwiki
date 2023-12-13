module Kidwiki.Lib where

import Kidwiki.Types
import Data.Char (toUpper, toLower)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Relude

mkAppSt :: IO AppSt
mkAppSt = AppSt <$> newManager tlsManagerSettings

mapHead :: (a -> a) -> [a] -> [a]
mapHead f = \case
  [] -> []
  (h:t) -> f h : t

capitilize :: [Char] -> [Char]
capitilize = mapHead toUpper

uncapitilize :: [Char] -> [Char]
uncapitilize = mapHead toLower
