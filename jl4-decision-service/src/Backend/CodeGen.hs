{-# LANGUAGE OverloadedStrings #-}
module Backend.CodeGen
  ( generateEvalWrapper
  , GeneratedCode(..)
  ) where

import Base
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import L4.Syntax (Type'(..), Resolved, getOriginal, rawName, rawNameToText)
import Backend.Api (TraceLevel(..))
import Backend.MaybeLift (liftTypeToMaybe)

-- | Result of code generation
data GeneratedCode = GeneratedCode
  { generatedWrapper :: Text
    -- ^ The L4 code to append after the filtered source
  , decodeFailedSentinel :: Text
    -- ^ The sentinel value to check for decode failure
  }
  deriving (Show, Eq)

-- | Generate L4 wrapper code for JSONDECODE-based evaluation
--
-- Uses deep Maybe lifting: ALL parameters are wrapped in MAYBE types,
-- allowing JSON null/missing values to decode to NOTHING.
--
-- For BOOLEAN parameters, uses fromMaybe FALSE to enable short-circuit
-- evaluation (if the boolean isn't needed, the default doesn't matter).
--
-- For non-BOOLEAN parameters, NOTHING propagates as an omitted/unknown value.
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
      -- Deep Maybe lifting: all parameters get MAYBE types
      let -- Check if a type is exactly BOOLEAN (not LIST OF BOOLEAN, etc.)
          -- Pattern match on TyApp with "Boolean" name and no type arguments
          isBooleanType :: Type' Resolved -> Bool
          isBooleanType (TyApp _ name []) =
            Text.toUpper (rawNameToText (rawName (getOriginal name))) == "BOOLEAN"
          isBooleanType _ = False
          -- Annotate params with their boolean status
          paramInfo = map (\(name, ty) -> ((name, ty), isBooleanType ty)) params
          -- We always need prelude for fromMaybe (all booleans use it)
          hasBooleans = any snd paramInfo
      in Right GeneratedCode
      { generatedWrapper = Text.unlines $
          [ ""
          , "-- ========== GENERATED WRAPPER (Deep Maybe Lifting) =========="
          ] ++
          -- Import prelude for fromMaybe if we have any booleans
          (if hasBooleans then ["IMPORT prelude  -- for fromMaybe"] else []) ++
          [ ""
          , generateInputRecordLifted params
          , ""
          , generateDecoder
          , ""
          , generateJsonPayload inputJson
          , ""
          , generateEvalDirectiveLifted funName paramInfo traceLevel
          ]
      , decodeFailedSentinel = "DECODE_FAILED"
      }

-- | Generate simple EVAL/EVALTRACE for zero-parameter functions
generateSimpleEval :: Text -> TraceLevel -> Text
generateSimpleEval funName traceLevel =
  case traceLevel of
    TraceNone -> "#EVAL " <> funName
    TraceFull -> "#EVALTRACE " <> funName

-- | Generate DECLARE for input record with ALL parameters lifted to MAYBE
-- This enables uniform handling of null/missing JSON values
generateInputRecordLifted :: [(Text, Type' Resolved)] -> Text
generateInputRecordLifted params = Text.unlines $
  ["DECLARE InputArgs HAS"] ++
  map formatField (zip [0::Int ..] params)
  where
    formatField (idx, (name, ty)) =
      let indent = if idx == 0 then "  " else ", "
          -- Lift ALL types to MAYBE
          tyText = liftTypeToMaybe ty
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

-- | Generate EVAL or EVALTRACE with deep Maybe lifting
--
-- For BOOLEAN parameters: uses fromMaybe FALSE for short-circuit evaluation
-- For non-BOOLEAN parameters: uses pattern matching to unwrap JUST values
--
-- The approach:
-- 1. Decode JSON to InputArgs (all fields are MAYBE types)
-- 2. For booleans: fromMaybe FALSE enables short-circuit (if not needed, default doesn't matter)
-- 3. For non-booleans: nested CONSIDER unwraps JUST values; NOTHING returns NOTHING
--
-- Type signature: [((name, type), isBoolean)]
generateEvalDirectiveLifted :: Text -> [((Text, Type' Resolved), Bool)] -> TraceLevel -> Text
generateEvalDirectiveLifted funName paramInfo traceLevel =
  let directive = case traceLevel of
        TraceNone -> "#EVAL"
        TraceFull -> "#EVALTRACE"
      -- Separate boolean and non-boolean params
      (_boolParams, nonBoolParams) = partition snd paramInfo
      -- Generate argument expressions in original order
      -- Booleans use fromMaybe FALSE directly
      -- Non-booleans reference unwrapped variable names
      allArgExprs = map snd $ sortOn fst $
        [(idx, expr) | (idx, (_, True)) <- zip [0..] paramInfo
                     , let ((name, _), _) = paramInfo !! idx
                           expr = "(fromMaybe FALSE (args's " <> name <> "))"] ++
        [(idx, expr) | (idx, (_, False)) <- zip [0..] paramInfo
                     , let ((name, _), _) = paramInfo !! idx
                           expr = "unwrapped_" <> name]
      functionCall = funName <> " " <> Text.unwords allArgExprs
  in if null nonBoolParams
     then -- All booleans: simple case, direct function call
       Text.unlines
         [ directive
         , "  CONSIDER decodeArgs inputJson"
         , "    WHEN RIGHT args THEN JUST (" <> functionCall <> ")"
         , "    WHEN LEFT error THEN NOTHING"
         ]
     else -- Has non-booleans: need nested CONSIDER for unwrapping
       Text.unlines $
         [ directive
         , "  CONSIDER decodeArgs inputJson"
         , "    WHEN RIGHT args THEN"
         ] ++
         generateNestedConsider nonBoolParams functionCall 3 ++
         [ "    WHEN LEFT error THEN NOTHING"
         ]

-- | Generate nested CONSIDER for unwrapping non-boolean MAYBE values
-- Each non-boolean param gets a CONSIDER that unwraps JUST or returns NOTHING
generateNestedConsider :: [((Text, Type' Resolved), Bool)] -> Text -> Int -> [Text]
generateNestedConsider [] functionCall indent =
  [Text.replicate indent "  " <> "JUST (" <> functionCall <> ")"]
generateNestedConsider (((name, _), _):rest) functionCall indent =
  let indentStr = Text.replicate indent "  "
  in [ indentStr <> "CONSIDER args's " <> name
     , indentStr <> "  WHEN JUST unwrapped_" <> name <> " THEN"
     ] ++
     generateNestedConsider rest functionCall (indent + 2) ++
     [ indentStr <> "  WHEN NOTHING THEN NOTHING"
     ]
