{-# LANGUAGE OverloadedStrings #-}
module Backend.CodeGen
  ( generateEvalWrapper
  , GeneratedCode(..)
  ) where

import Base
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
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
-- Supports partial parameters for simple BOOLEAN types:
-- - All provided parameters use their original types
-- - Missing BOOLEAN parameters use MAYBE BOOLEAN and fromMaybe with FALSE default
-- - For short-circuit evaluation, the default won't affect the result when the value isn't needed
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
    else
      -- Partition parameters into provided (in JSON) and missing
      let providedKeys = case inputJson of
            Aeson.Object obj -> map Key.toText (KM.keys obj)
            _ -> []
          -- Check which params are missing and if they're simple BOOLEAN types
          paramInfo = map (\p@(name, ty) ->
            let isMissing = name `notElem` providedKeys
                isBooleanType = "BOOLEAN" `Text.isInfixOf` Text.toUpper (prettyLayout ty)
            in (p, isMissing, isBooleanType)) params
          -- Only use MAYBE for missing BOOLEAN parameters
          hasMissingBoolean = any (\(_, isMissing, isBool) -> isMissing && isBool) paramInfo
      in Right GeneratedCode
      { generatedWrapper = Text.unlines $
          [ ""
          , "-- ========== GENERATED WRAPPER =========="
          ] ++
          -- Only import prelude if we need fromMaybe
          (if hasMissingBoolean then ["IMPORT prelude  -- for fromMaybe"] else []) ++
          [ ""
          , generateInputRecordPartial paramInfo
          , ""
          , generateDecoder
          , ""
          , generateJsonPayload inputJson
          , ""
          , generateEvalDirectivePartial funName paramInfo traceLevel
          ]
      , decodeFailedSentinel = "DECODE_FAILED"
      }

-- | Generate simple EVAL/EVALTRACE for zero-parameter functions
generateSimpleEval :: Text -> TraceLevel -> Text
generateSimpleEval funName traceLevel =
  case traceLevel of
    TraceNone -> "#EVAL " <> funName
    TraceFull -> "#EVALTRACE " <> funName

-- | Generate DECLARE for input record with MAYBE types only for missing BOOLEAN params
-- Type signature: [((name, type), isMissing, isBoolean)]
generateInputRecordPartial :: [((Text, Type' Resolved), Bool, Bool)] -> Text
generateInputRecordPartial paramInfo = Text.unlines $
  ["DECLARE InputArgs HAS"] ++
  map formatField (zip [0::Int ..] paramInfo)
  where
    formatField (idx, ((name, ty), isMissing, isBool)) =
      let indent = if idx == 0 then "  " else ", "
          -- Use MAYBE type only for missing BOOLEAN parameters
          tyText = if isMissing && isBool
                   then "MAYBE " <> prettyLayout ty
                   else prettyLayout ty
      in indent <> name <> " IS A " <> tyText

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

-- | Generate EVAL or EVALTRACE with fromMaybe unwrapping only for missing BOOLEAN params
-- Type signature: [((name, type), isMissing, isBoolean)]
generateEvalDirectivePartial :: Text -> [((Text, Type' Resolved), Bool, Bool)] -> TraceLevel -> Text
generateEvalDirectivePartial funName paramInfo traceLevel = Text.unlines $
  [ directive
  , "  CONSIDER decodeArgs inputJson"
  , "    WHEN RIGHT args THEN JUST (" <> functionCall <> ")"
  , "    WHEN LEFT error THEN NOTHING"
  ]
  where
    directive = case traceLevel of
      TraceNone -> "#EVAL"
      TraceFull -> "#EVALTRACE"
    functionCall = funName <> " " <> Text.unwords (map mkArgAccess paramInfo)
    -- Use fromMaybe only for missing BOOLEAN parameters
    mkArgAccess ((name, _ty), isMissing, isBool)
      | isMissing && isBool = "(fromMaybe FALSE (args's " <> name <> "))"
      | otherwise = "(args's " <> name <> ")"
