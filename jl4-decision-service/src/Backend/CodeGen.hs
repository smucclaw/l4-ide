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
import L4.Syntax (Type'(..), Resolved, getUnique)
import L4.TypeCheck.Environment (booleanUnique)
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
--
-- ASSUME params are injected as LET bindings before the function call,
-- shadowing any global ASSUME declarations with the provided values.
generateEvalWrapper
  :: Text                         -- ^ Target function name
  -> [(Text, Type' Resolved)]     -- ^ GIVEN parameter names and types (passed as function args)
  -> [(Text, Type' Resolved)]     -- ^ ASSUME parameter names and types (injected as LET bindings)
  -> Aeson.Value                  -- ^ Input arguments as JSON object
  -> TraceLevel                   -- ^ Whether to generate EVAL or EVALTRACE
  -> Either Text GeneratedCode
generateEvalWrapper funName givenParams assumeParams inputJson traceLevel = do
  let allParams = givenParams <> assumeParams
  -- Handle zero-parameter functions: no wrapper needed, just eval directly
  if null allParams
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
          -- Compare unique symbolically rather than via string comparison
          isBooleanType :: Type' Resolved -> Bool
          isBooleanType (TyApp _ name []) = getUnique name == booleanUnique
          isBooleanType _ = False
          -- Annotate GIVEN params with their boolean status and mark as GIVEN
          givenParamInfo = map (\(name, ty) -> ((name, ty), isBooleanType ty, True)) givenParams
          -- Annotate ASSUME params with their boolean status and mark as ASSUME
          assumeParamInfo = map (\(name, ty) -> ((name, ty), isBooleanType ty, False)) assumeParams
          allParamInfo = givenParamInfo <> assumeParamInfo
          -- We always need prelude for fromMaybe (all booleans use it)
          hasBooleans = any (\(_, isB, _) -> isB) allParamInfo
      in Right GeneratedCode
      { generatedWrapper = Text.unlines $
          [ ""
          , "-- ========== GENERATED WRAPPER (Deep Maybe Lifting) =========="
          ] ++
          -- Import prelude for fromMaybe if we have any booleans
          (if hasBooleans then ["IMPORT prelude  -- for fromMaybe"] else []) ++
          [ ""
          , generateInputRecordLifted allParams
          , ""
          , generateDecoder
          , ""
          , generateJsonPayload inputJson
          , ""
          , generateEvalDirectiveLiftedWithAssumes funName givenParamInfo assumeParamInfo traceLevel
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

-- | Generate EVAL or EVALTRACE with deep Maybe lifting, handling both GIVEN and ASSUME params.
--
-- GIVEN params are passed as function arguments.
-- ASSUME params are injected as LET bindings before the function call.
--
-- Type signature: [((name, type), isBoolean, isGiven)]
generateEvalDirectiveLiftedWithAssumes
  :: Text
  -> [((Text, Type' Resolved), Bool, Bool)]  -- ^ GIVEN params: ((name, type), isBoolean, isGiven=True)
  -> [((Text, Type' Resolved), Bool, Bool)]  -- ^ ASSUME params: ((name, type), isBoolean, isGiven=False)
  -> TraceLevel
  -> Text
generateEvalDirectiveLiftedWithAssumes funName givenParamInfo assumeParamInfo traceLevel =
  let directive = case traceLevel of
        TraceNone -> "#EVAL"
        TraceFull -> "#EVALTRACE"

      -- All params for unwrapping
      allParams = givenParamInfo <> assumeParamInfo

      -- Separate boolean and non-boolean params (for unwrapping logic)
      nonBoolAllParams = filter (\(_, isB, _) -> not isB) allParams

      -- Generate argument expressions for GIVEN params only (in original order)
      -- Booleans use fromMaybe FALSE directly
      -- Non-booleans reference unwrapped variable names
      givenArgExprs = map snd $ sortOn fst $
        [(idx, expr) | (idx, ((name, _), True, _)) <- zip [0 :: Int ..] givenParamInfo
                     , let expr = "(fromMaybe FALSE (args's " <> name <> "))"] ++
        [(idx, expr) | (idx, ((name, _), False, _)) <- zip [0 :: Int ..] givenParamInfo
                     , let expr = "unwrapped_" <> name]

      -- Build function call with only GIVEN params as arguments
      functionCall = if null givenArgExprs
        then funName
        else funName <> " " <> Text.unwords givenArgExprs

      -- Wrap function call with LET bindings for ASSUME params
      -- Boolean ASSUMEs use fromMaybe FALSE
      -- Non-boolean ASSUMEs use unwrapped_ variables
      wrapWithAssumes :: Text -> Text
      wrapWithAssumes innerExpr = foldr wrapOne innerExpr assumeParamInfo
        where
          wrapOne ((name, _), isBoolean, _) expr =
            let valueExpr = if isBoolean
                  then "(fromMaybe FALSE (args's " <> name <> "))"
                  else "unwrapped_" <> name
            in "LET " <> name <> " = " <> valueExpr <> " IN " <> expr

      -- The innermost expression (wrapped function call in JUST)
      innerCall = "JUST (" <> wrapWithAssumes functionCall <> ")"

  in if null nonBoolAllParams
     then -- All booleans: simple case, no nested CONSIDER needed
       Text.unlines
         [ directive
         , "  CONSIDER decodeArgs inputJson"
         , "    WHEN RIGHT args THEN " <> innerCall
         , "    WHEN LEFT error THEN NOTHING"
         ]
     else -- Has non-booleans: need nested CONSIDER for unwrapping
       Text.unlines $
         [ directive
         , "  CONSIDER decodeArgs inputJson"
         , "    WHEN RIGHT args THEN"
         ] ++
         generateNestedConsiderWithAssumes nonBoolAllParams functionCall assumeParamInfo 3 ++
         [ "    WHEN LEFT error THEN NOTHING"
         ]

-- | Generate nested CONSIDER for unwrapping non-boolean MAYBE values,
-- with LET bindings for ASSUME params at the innermost level.
generateNestedConsiderWithAssumes
  :: [((Text, Type' Resolved), Bool, Bool)]  -- ^ Non-boolean params to unwrap
  -> Text                                     -- ^ Function call (GIVEN args only)
  -> [((Text, Type' Resolved), Bool, Bool)]  -- ^ ASSUME params for LET bindings
  -> Int                                      -- ^ Indentation level
  -> [Text]
generateNestedConsiderWithAssumes [] functionCall assumeParamInfo indent =
  let indentStr = Text.replicate indent "  "
      -- Wrap function call with LET bindings for ASSUME params
      wrapWithAssumes :: Text -> Text
      wrapWithAssumes innerExpr = foldr wrapOne innerExpr assumeParamInfo
        where
          wrapOne ((name, _), isBoolean, _) expr =
            let valueExpr = if isBoolean
                  then "(fromMaybe FALSE (args's " <> name <> "))"
                  else "unwrapped_" <> name
            in "LET " <> name <> " = " <> valueExpr <> " IN " <> expr
  in [indentStr <> "JUST (" <> wrapWithAssumes functionCall <> ")"]
generateNestedConsiderWithAssumes (((name, _), _, _):rest) functionCall assumeParamInfo indent =
  let indentStr = Text.replicate indent "  "
  in [ indentStr <> "CONSIDER args's " <> name
     , indentStr <> "  WHEN JUST unwrapped_" <> name <> " THEN"
     ] ++
     generateNestedConsiderWithAssumes rest functionCall assumeParamInfo (indent + 2) ++
     [ indentStr <> "  WHEN NOTHING THEN NOTHING"
     ]
