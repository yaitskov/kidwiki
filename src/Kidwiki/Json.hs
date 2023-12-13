{-# LANGUAGE AllowAmbiguousTypes #-}
module Kidwiki.Json where

import Control.Lens
import Data.Aeson as A
import Data.Aeson.KeyMap qualified as DAKM
import Data.Aeson.Types
import Data.Char
import Data.Map.Strict qualified as M
import Data.Text.Encoding qualified as DTE
import GHC.Generics
import Relude

import Language.Haskell.TH.Syntax


type Balast = Int

cutPrefix :: String -> A.Options -> A.Options
cutPrefix prefix o
  = o
      { fieldLabelModifier =
          (ix 0 %~ toLower) . drop (length prefix)
      }

cutNamePrefix :: Name -> A.Options -> A.Options
cutNamePrefix (Name s _ ) = cutPrefix (occString s)

genericDbNonSumTypeToJson ::
  forall a.
  (Generic a, GToJSON' Value Zero (Rep a)) =>
  Name -> a -> Value
genericDbNonSumTypeToJson n = A.genericToJSON (cutNamePrefix n psTypeAliasArgonautOptions)

genericDbNonSumTypeParseJson ::
  forall a.
  (Generic a, GFromJSON Zero (Rep a)) =>
  Name -> Value -> Parser a
genericDbNonSumTypeParseJson n = A.genericParseJSON (cutNamePrefix n psTypeAliasArgonautOptions)

argonautTaggedObject :: SumEncoding
argonautTaggedObject = A.TaggedObject "tag" "value"

-- | Aeson and Agronaut encodings for equivalent types have lot of
-- subtle differences like these libraries exist in different universes
-- rewriting JSON adapter on Agronaut side needs help from Aeson side
-- to avoid ambiguity.
-- The ideal solution to this workaround is to reanimate purescript-bridge
-- haskell library.
-- All user types should use it.
-- Supported systems types: (), Int, Double, String, Maybe, [], Either, though
-- instead of Maps use JsonMap wrapper
argonautOptions :: Options
argonautOptions =
  psTypeAliasArgonautOptions
    { sumEncoding = argonautTaggedObject
    -- no effect on Either DCs:
    -- prefix need to disambiguate const DC from regular strings
    , constructorTagModifier = ("dc:" <>)
    -- Argonaut requires tag even for non-sum type
    , tagSingleConstructors = True
    }

-- PureScript cannot produce generic Argonaout marshals for record type alias.
-- record wrapped into newtype doesn't produce meta info (tag)
-- and data/newtype complicates access to fields
-- Manually encoded records don't have meta info too.
-- Keep Ends in sync
psTypeAliasArgonautOptions :: Options
psTypeAliasArgonautOptions = A.defaultOptions

decodeTextAsJson :: FromJSON a => Text -> Either String a
decodeTextAsJson = A.eitherDecode . toLazy . DTE.encodeUtf8

encodeJsonAsText :: ToJSON a => a -> Text
encodeJsonAsText = DTE.decodeUtf8 . toStrict . A.encode

-- Aeson encodes maps as plain JSON object which are not distinguishable
-- from serialized PureScript type aliases at wrapping/unwrapping keys
--into/from "value" subobject
newtype JsonMap k v = JsonMap { unJsonMap :: M.Map k v }
  deriving newtype (Eq, Show)

class JsonKeyAsString a where
  valueToString :: a -> String
instance JsonKeyAsString String where
  valueToString = id

instance JsonKeyAsString Int where
  valueToString = show
instance JsonKeyAsString Int32 where
  valueToString = show

instance
  (JsonKeyAsString k, ToJSONKey k, ToJSON v) =>
    ToJSON (JsonMap k v) where
  toJSON (JsonMap m) =
    Object $
      DAKM.singleton (fromString "$map") $
        toJSON m -- $
          -- M.mapKeys valueToString m

instance
  (JsonKeyAsString k, Ord k, FromJSONKey k, FromJSON v) =>
    FromJSON (JsonMap k v) where
  parseJSON = \case
    Object m ->
      case DAKM.lookup "$map" m of
        Nothing -> fail $ "no $map key in " <> show m
        Just mm -> JsonMap <$> parseJSON mm
    o -> fail $ "expected object but " <> show o
