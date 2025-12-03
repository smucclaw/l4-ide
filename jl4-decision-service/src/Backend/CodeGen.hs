{-# LANGUAGE OverloadedStrings #-}
module Backend.CodeGen
  ( generateEvalWrapper
  , generateEvalWrapperWithDefaults
  , GeneratedCode(..)
  ) where

import Base
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import L4.Syntax (Type'(..), Resolved)
import L4.Print (prettyLayout)
import Backend.Api (TraceLevel(..))

-- | Result of code generation
data GeneratedCode = GeneratedCode
  { generatedWrapper :: Text
    -- ^ The L4 code to append after the filtered source
  , decodeFailedSentinel :: Text
    -- ^ The sentinel value to check for decode failure
  }
  deriving (Show, Eq)

-- | Generate L4 wrapper code for JSONDECODE-based evaluation
generateEvalWrapper
  :: Text                         -- ^ Target function name
  -> [(Text, Type' Resolved)]     -- ^ Parameter names and types from GIVEN clause
  -> Aeson.Value                  -- ^ Input arguments as JSON object
  -> TraceLevel                   -- ^ Whether to generate EVAL or EVALTRACE
  -> Either Text GeneratedCode
generateEvalWrapper funName params inputJson traceLevel = do
  -- Handle zero-parameter functions: no wrapper needed, just eval directly
  if null params
    then Right GeneratedCode
      { generatedWrapper = Text.unlines
          [ ""
          , "-- ========== GENERATED WRAPPER =========="
          , ""
          , generateSimpleEval funName traceLevel
          ]
      , decodeFailedSentinel = "DECODE_FAILED"
      }
    else Right GeneratedCode
      { generatedWrapper = Text.unlines
          [ ""
          , "-- ========== GENERATED WRAPPER =========="
          , ""
          , generateInputRecord params
          , ""
          , generateDecoder
          , ""
          , generateJsonPayload inputJson
          , ""
          , generateEvalDirective funName params traceLevel
          ]
      , decodeFailedSentinel = "DECODE_FAILED"
      }

-- | Generate simple EVAL/EVALTRACE for zero-parameter functions
generateSimpleEval :: Text -> TraceLevel -> Text
generateSimpleEval funName traceLevel =
  case traceLevel of
    TraceNone -> "#EVAL " <> funName
    TraceFull -> "#EVALTRACE " <> funName

-- | Generate DECLARE for input record
generateInputRecord :: [(Text, Type' Resolved)] -> Text
generateInputRecord params = Text.unlines $
  ["DECLARE InputArgs HAS"] ++
  map formatField (zip [0::Int ..] params)
  where
    formatField (idx, (name, ty)) =
      let indent = if idx == 0 then "  " else ", "
      in indent <> name <> " IS A " <> prettyLayout ty

-- | Generate typed decoder function
generateDecoder :: Text
generateDecoder = Text.unlines
  [ "GIVEN jsn IS A STRING"
  , "GIVETH AN EITHER STRING InputArgs"
  , "decodeArgs jsn MEANS JSONDECODE jsn"
  ]

-- | Generate JSON payload as L4 string literal
generateJsonPayload :: Aeson.Value -> Text
generateJsonPayload json =
  "DECIDE inputJson IS " <> escapeAsL4String json

-- | Escape JSON value as an L4 string literal
escapeAsL4String :: Aeson.Value -> Text
escapeAsL4String val =
  let jsonText = TL.toStrict $ TL.decodeUtf8 $ Aeson.encode val
      -- Only escape internal quotes - JSON is already properly formatted
      escaped = Text.replace "\"" "\\\"" jsonText
  in "\"" <> escaped <> "\""

-- | Generate EVAL or EVALTRACE with CONSIDER/WHEN unwrapper
generateEvalDirective :: Text -> [(Text, Type' Resolved)] -> TraceLevel -> Text
generateEvalDirective funName params traceLevel = Text.unlines $
  [ directive
  , "  CONSIDER decodeArgs inputJson"
  , "    WHEN RIGHT args THEN JUST (" <> functionCall <> ")"
  , "    WHEN LEFT error THEN NOTHING"
  ]
  where
    directive = case traceLevel of
      TraceNone -> "#EVAL"
      TraceFull -> "#EVALTRACE"
    functionCall = funName <> " " <> Text.unwords (map mkArgAccess params)
    mkArgAccess (name, _) = "(args's " <> name <> ")"

-- | Generate L4 wrapper code with TYPICALLY defaults support
-- When honorDefaults is True, parameters with defaults are substituted when missing from JSON
generateEvalWrapperWithDefaults
  :: Text                                      -- ^ Target function name
  -> [(Text, Type' Resolved, Maybe Text)]      -- ^ Parameter names, types, and optional L4 default expressions
  -> Aeson.Value                               -- ^ Input arguments as JSON object
  -> TraceLevel                                -- ^ Whether to generate EVAL or EVALTRACE
  -> Bool                                      -- ^ Whether to honor TYPICALLY defaults
  -> Either Text GeneratedCode
generateEvalWrapperWithDefaults funName paramsWithDefaults inputJson traceLevel honorDefaults = do
  let params = [(name, ty) | (name, ty, _) <- paramsWithDefaults]
      defaults = [(name, def) | (name, _, Just def) <- paramsWithDefaults]
  
  if null params
    then Right GeneratedCode
      { generatedWrapper = Text.unlines
          [ ""
          , "-- ========== GENERATED WRAPPER =========="
          , ""
          , generateSimpleEval funName traceLevel
          ]
      , decodeFailedSentinel = "DECODE_FAILED"
      }
    else if not honorDefaults || null defaults
      then generateEvalWrapper funName params inputJson traceLevel
      else Right GeneratedCode
        { generatedWrapper = Text.unlines
            [ ""
            , "-- ========== GENERATED WRAPPER (presumptive) =========="
            , ""
            , generateInputRecordWithDefaults paramsWithDefaults
            , ""
            , generateDecoder
            , ""
            , generateJsonPayload inputJson
            , ""
            , generateEvalDirectiveWithDefaults funName paramsWithDefaults traceLevel
            ]
        , decodeFailedSentinel = "DECODE_FAILED"
        }

-- | Generate DECLARE for input record with TYPICALLY defaults
generateInputRecordWithDefaults :: [(Text, Type' Resolved, Maybe Text)] -> Text
generateInputRecordWithDefaults params = Text.unlines $
  ["DECLARE InputArgs HAS"] ++
  map formatField (zip [0::Int ..] params)
  where
    formatField (idx, (name, ty, mDefault)) =
      let indent = if idx == 0 then "  " else ", "
          defaultClause = case mDefault of
            Just def -> " TYPICALLY " <> def
            Nothing -> ""
      in indent <> name <> " IS A " <> prettyLayout ty <> defaultClause

-- | Generate EVAL with defaults-aware function call
generateEvalDirectiveWithDefaults :: Text -> [(Text, Type' Resolved, Maybe Text)] -> TraceLevel -> Text
generateEvalDirectiveWithDefaults funName params traceLevel = Text.unlines $
  [ directive
  , "  CONSIDER decodeArgs inputJson"
  , "    WHEN RIGHT args THEN JUST (" <> functionCall <> ")"
  , "    WHEN LEFT error THEN NOTHING"
  ]
  where
    directive = case traceLevel of
      TraceNone -> "#EVAL"
      TraceFull -> "#EVALTRACE"
    functionCall = funName <> " " <> Text.unwords (map mkArgAccess params)
    mkArgAccess (name, _, _) = "(args's " <> name <> ")"
