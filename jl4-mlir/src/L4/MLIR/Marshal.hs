{-# LANGUAGE OverloadedStrings #-}

-- | JSON request/response types and validation for @jl4-mlir run@.
--
-- These types mirror @Backend.Api@ in @jl4-service@ exactly so that a
-- compiled @.wasm@ module can be invoked through the same JSON
-- protocol the service HTTP API uses. Keeping the types in this
-- package (rather than depending on @jl4-service@) avoids pulling in
-- the service's heavy deps (Servant, Warp, CBOR, etc.) just to run a
-- tiny WASM.
--
-- == Wire compatibility
-- Every field name, JSON shape, and parsing rule matches
-- @jl4-service/src/Backend/Api.hs@. If the service changes its wire
-- format, update both sides together.
module L4.MLIR.Marshal
  ( -- * Request types (match Backend.Api.FnArguments)
    FnArguments (..)
  , FnLiteral (..)
  , TraceEvent (..)

    -- * Response types (match Backend.Api.ResponseWithReason)
  , ResponseWithReason (..)
  , Reasoning (..)
  , emptyReasoning

    -- * Validation / routing
  , ResolvedArgs (..)
  , resolveArgs

    -- * Argument remapping (sanitized ↔ original)
  , remapArguments
  , buildPropertyReverseMap
  ) where

import Control.Applicative ((<|>))
import Data.Aeson ((.:), (.:?), (.!=), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson.Key
import qualified Data.Aeson.KeyMap as Aeson.KeyMap
import Data.Bifunctor (bimap)
import qualified Data.Foldable as Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Scientific as Scientific
import Data.Text (Text)
import qualified Data.Vector as V

import L4.FunctionSchema (Parameter(..), Parameters(..))
import L4.MLIR.Schema (FunctionExport(..), sanitizePropertyName)

-- ---------------------------------------------------------------------------
-- FnLiteral — runtime values (matches Backend.Api.FnLiteral)
-- ---------------------------------------------------------------------------

data FnLiteral
  = FnLitInt !Integer
  | FnLitDouble !Double
  | FnLitBool !Bool
  | FnLitString !Text
  | FnArray [FnLiteral]
  | FnObject [(Text, FnLiteral)]
  | FnUncertain
  | FnUnknown
  deriving (Show, Eq)

instance Aeson.ToJSON FnLiteral where
  toJSON = \case
    FnLitInt v    -> Aeson.Number (fromIntegral v)
    FnLitDouble v -> Aeson.Number (Scientific.fromFloatDigits v)
    FnLitBool v   -> Aeson.Bool v
    FnLitString v -> Aeson.String v
    FnArray vs    -> Aeson.Array (V.fromList (map Aeson.toJSON vs))
    FnObject ps   -> Aeson.Object $ Aeson.KeyMap.fromList $
      fmap (bimap Aeson.Key.fromText Aeson.toJSON) ps
    FnUncertain   -> Aeson.Object (Aeson.KeyMap.fromList [])
    FnUnknown     -> Aeson.Null

instance Aeson.FromJSON FnLiteral where
  parseJSON = \case
    Aeson.String v -> pure $ FnLitString v
    Aeson.Bool v   -> pure $ FnLitBool v
    Aeson.Number v
      | Scientific.isInteger v -> pure $ FnLitInt (Scientific.coefficient v * (10 ^ Scientific.base10Exponent v))
      | Right d <- Scientific.toBoundedRealFloat v -> pure $ FnLitDouble d
      | otherwise -> fail "Unrepresentable number"
    Aeson.Null     -> pure FnUnknown
    Aeson.Array vs -> FnArray <$> traverse Aeson.parseJSON (Foldable.toList vs)
    Aeson.Object o
      | null (Aeson.KeyMap.toList o) -> pure FnUncertain
      | otherwise -> do
          ps <- traverse (\(k, v) -> (Aeson.Key.toText k,) <$> Aeson.parseJSON v)
                         (Aeson.KeyMap.toList o)
          pure $ FnObject ps

-- ---------------------------------------------------------------------------
-- TraceEvent (for DEONTIC function calls)
-- ---------------------------------------------------------------------------

data TraceEvent = TraceEvent
  { teParty  :: !FnLiteral
  , teAction :: !FnLiteral
  , teAt     :: !Scientific.Scientific
  }
  deriving (Show, Eq)

instance Aeson.FromJSON TraceEvent where
  parseJSON = Aeson.withObject "TraceEvent" $ \o ->
    TraceEvent
      <$> o .: "party"
      <*> o .: "action"
      <*> o .: "at"

instance Aeson.ToJSON TraceEvent where
  toJSON te = Aeson.object
    [ "party"  .= te.teParty
    , "action" .= te.teAction
    , "at"     .= te.teAt
    ]

-- ---------------------------------------------------------------------------
-- FnArguments (HTTP request body)
-- ---------------------------------------------------------------------------

data FnArguments = FnArguments
  { faArguments :: Map Text (Maybe FnLiteral)
  , faStartTime :: Maybe Scientific.Scientific
  , faEvents    :: Maybe [TraceEvent]
  }
  deriving (Show, Eq)

instance Aeson.FromJSON FnArguments where
  parseJSON = Aeson.withObject "FnArguments" $ \o ->
    FnArguments
      <$> o .: "arguments"
      <*> o .:? "startTime"
      <*> o .:? "events"

instance Aeson.ToJSON FnArguments where
  toJSON fa = Aeson.object $
    [ "arguments" .= fa.faArguments ]
    <> maybe [] (\t -> ["startTime" .= t]) fa.faStartTime
    <> maybe [] (\e -> ["events" .= e]) fa.faEvents

-- ---------------------------------------------------------------------------
-- Response
-- ---------------------------------------------------------------------------

data Reasoning = Reasoning
  { rExampleCode :: [Text]
  , rExplanation :: [Text]
  , rChildren    :: [Reasoning]
  }
  deriving (Show, Eq)

emptyReasoning :: Reasoning
emptyReasoning = Reasoning [] [] []

instance Aeson.ToJSON Reasoning where
  toJSON r = Aeson.object
    [ "exampleCode" .= r.rExampleCode
    , "explanation" .= r.rExplanation
    , "children"    .= r.rChildren
    ]

instance Aeson.FromJSON Reasoning where
  parseJSON = Aeson.withObject "Reasoning" $ \o ->
    Reasoning
      <$> o .:? "exampleCode" .!= []
      <*> o .:? "explanation" .!= []
      <*> o .:? "children" .!= []

-- | Evaluation result. Matches @Backend.Api.ResponseWithReason@.
--
-- The @result@ key holds a map from field name to FnLiteral so that
-- multi-output functions (records, DEONTIC responses, etc.) can return
-- structured data with the same shape as the input @arguments@.
data ResponseWithReason = ResponseWithReason
  { rrResult    :: Map Text FnLiteral
  , rrReasoning :: Reasoning
  }
  deriving (Show, Eq)

instance Aeson.ToJSON ResponseWithReason where
  toJSON rr = Aeson.object $
    [ "result" .= rr.rrResult ]
    <> (if isEmptyReasoning rr.rrReasoning then [] else ["reasoning" .= rr.rrReasoning])
    where
      isEmptyReasoning r =
        null r.rExampleCode && null r.rExplanation && null r.rChildren

instance Aeson.FromJSON ResponseWithReason where
  parseJSON = Aeson.withObject "ResponseWithReason" $ \o ->
    ResponseWithReason
      <$> (o .: "result" <|> o .: "fnResult")
      <*> o .:? "reasoning" .!= emptyReasoning

-- ---------------------------------------------------------------------------
-- Validation & resolution
-- ---------------------------------------------------------------------------

-- | Arguments resolved against a function schema, ready to be marshaled
-- into a WASM call. Arguments are laid out in the schema's declared
-- @paramOrder@ so the generated JS runtime can call them positionally.
data ResolvedArgs = ResolvedArgs
  { raPositional :: [(Text, Parameter, Maybe FnLiteral)]
    -- ^ @(paramName, schema, provided value)@ in declaration order
  , raStartTime  :: Maybe Scientific.Scientific
  , raEvents     :: Maybe [TraceEvent]
  }
  deriving (Show)

-- | Validate incoming @FnArguments@ against the function's schema and
-- produce a positional argument list for the WASM call.
--
-- Accepts both sanitized (hyphenated) and original (spaced) parameter
-- names, matching jl4-service's @remapArguments@ behavior.
resolveArgs :: FunctionExport -> FnArguments -> Either Text ResolvedArgs
resolveArgs fe args = do
  let params = fe.parameters
      paramMap = params.parameterMap
      reverseMap = buildPropertyReverseMap params
      remappedArgs = remapArguments reverseMap (Map.toList args.faArguments)
      providedByName = Map.fromList remappedArgs

      -- Build positional list from paramOrder
      positional =
        [ (pname, Map.findWithDefault (missingParam pname) pname paramMap, Map.lookup pname providedByName >>= id)
        | pname <- fe.paramOrder
        ]

      missingRequired =
        [ pname
        | pname <- params.required
        , Map.notMember pname providedByName
        ]

  if not (null missingRequired)
    then Left $ "Missing required parameters: "
      <> mconcat [x <> (if i < length missingRequired - 1 then ", " else "")
                  | (i, x) <- zip [0 :: Int ..] missingRequired]
    else
      if fe.isDeontic
        then case (args.faStartTime, args.faEvents) of
          (Nothing, _) -> Left "startTime is required for DEONTIC functions"
          (_, Nothing) -> Left "events is required for DEONTIC functions"
          (Just _, Just _) -> Right $ ResolvedArgs positional args.faStartTime args.faEvents
        else case (args.faStartTime, args.faEvents) of
          (Just _, _) -> Left "startTime is only valid for DEONTIC functions"
          (_, Just _) -> Left "events is only valid for DEONTIC functions"
          _ -> Right $ ResolvedArgs positional Nothing Nothing
  where
    missingParam _ = Parameter "object" Nothing Nothing [] "" Nothing Nothing Nothing Nothing

-- ---------------------------------------------------------------------------
-- Name remapping (matches jl4-service/src/Shared.hs)
-- ---------------------------------------------------------------------------

-- | Build a reverse map from sanitized property names back to originals.
-- Only records names where sanitization is non-trivial (i.e., the
-- sanitized name differs from the original), so identity lookups are
-- handled by @Map.findWithDefault k k@.
buildPropertyReverseMap :: Parameters -> Map Text Text
buildPropertyReverseMap (MkParameters props _) =
  Map.fromList
    [ (sanitizePropertyName k, k)
    | k <- Map.keys props
    , sanitizePropertyName k /= k
    ]

-- | Remap top-level argument keys using a sanitized → original map.
remapArguments :: Map Text Text -> [(Text, Maybe FnLiteral)] -> [(Text, Maybe FnLiteral)]
remapArguments reverseMap args =
  [ (Map.findWithDefault k k reverseMap, fmap (remapFnLiteralKeys reverseMap) v)
  | (k, v) <- args
  ]

-- | Recursively remap keys in nested FnObject / FnArray values.
remapFnLiteralKeys :: Map Text Text -> FnLiteral -> FnLiteral
remapFnLiteralKeys reverseMap = go
  where
    go (FnObject ps) = FnObject [(Map.findWithDefault k k reverseMap, go v) | (k, v) <- ps]
    go (FnArray vs)  = FnArray (map go vs)
    go other = other
