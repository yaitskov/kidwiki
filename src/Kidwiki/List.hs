module Kidwiki.List where

import Relude

mapFirst :: (a -> Maybe a) -> [ a ] -> Maybe [ a ]
mapFirst f = \case
  [] -> Nothing
  (h:r) ->
    case f h of
      Just h' -> Just $ h' : r
      Nothing ->
        case mapFirst f r of
          Just r' -> Just $ h : r'
          Nothing -> Nothing

chunksOf :: Int -> [e] -> [[e]]
chunksOf i ls = map (take i) (build (splitter ls)) where
  splitter :: [e] -> ([e] -> a -> a) -> a -> a
  splitter [] _ n = n
  splitter l c n  = l `c` splitter (drop i l) c n

  build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
  build g = g (:) []

class Chunkable a where
  cutInChunks :: Int -> a -> [a]

instance Chunkable [a] where
  cutInChunks = chunksOf

mayHead :: [a] -> Maybe a
mayHead (a:_) = Just a
mayHead _ = Nothing
