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
import L4.TypeCheck.Environment (booleanUnique, dateUnique)
import Backend.Api (TraceLevel(..))
import Backend.MaybeLift (liftTypeToMaybe)

-- | Check if a type is exactly DATE (not MAYBE DATE, LIST OF DATE, etc.)
-- Used to determine if we need TODATE conversion from JSON strings
isDateType :: Type' Resolved -> Bool
isDateType (TyApp _ name []) = getUnique name == dateUnique
isDateType _ = False

-- | Check if a type contains DATE at its core (unwrapping MAYBE)
-- e.g., DATE -> True, MAYBE DATE -> True, MAYBE (MAYBE DATE) -> True
-- but LIST OF DATE -> False (we don't handle nested structures yet)
containsDateType :: Type' Resolved -> Bool
containsDateType ty = isDateType (unwrapMaybe ty)
  where
    -- Unwrap MAYBE layers to get to the core type
    -- Note: We can't easily check for maybeUnique here without importing it,
    -- so we use a heuristic: if it's a TyApp with one type argument, check inner
    unwrapMaybe :: Type' Resolved -> Type' Resolved
    unwrapMaybe (TyApp _ _ [inner]) = unwrapMaybe inner
    unwrapMaybe t = t

-- | Quote an identifier with backticks if it contains spaces or special characters
-- L4 requires backticks for identifiers with spaces
quoteIdent :: Text -> Text
quoteIdent name
  | Text.any (== ' ') name = "`" <> name <> "`"
  | otherwise = name

-- | Create a safe unwrapped variable name
-- For names with spaces, creates a backtick-quoted identifier like `unwrapped_tranche number`
unwrappedVar :: Text -> Text
unwrappedVar name
  | Text.any (== ' ') name = "`unwrapped_" <> name <> "`"
  | otherwise = "unwrapped_" <> name

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
          -- Annotate GIVEN params with (type info, isBoolean, isGiven, isDate)
          givenParamInfo = map (\(name, ty) -> ((name, ty), isBooleanType ty, True, containsDateType ty)) givenParams
          -- Annotate ASSUME params with (type info, isBoolean, isGiven, isDate)
          assumeParamInfo = map (\(name, ty) -> ((name, ty), isBooleanType ty, False, containsDateType ty)) assumeParams
          allParamInfo = givenParamInfo <> assumeParamInfo
          -- We always need prelude for fromMaybe (all booleans use it)
          hasBooleans = any (\(_, isB, _, _) -> isB) allParamInfo
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
          -- Quote identifiers with spaces
          quotedName = quoteIdent name
      in indent <> quotedName <> " IS A " <> tyText

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
-- Type signature: [((name, type), isBoolean, isGiven, isDate)]
generateEvalDirectiveLiftedWithAssumes
  :: Text
  -> [((Text, Type' Resolved), Bool, Bool, Bool)]  -- ^ GIVEN params: ((name, type), isBoolean, isGiven=True, isDate)
  -> [((Text, Type' Resolved), Bool, Bool, Bool)]  -- ^ ASSUME params: ((name, type), isBoolean, isGiven=False, isDate)
  -> TraceLevel
  -> Text
generateEvalDirectiveLiftedWithAssumes funName givenParamInfo assumeParamInfo traceLevel =
  let directive = case traceLevel of
        TraceNone -> "#EVAL"
        TraceFull -> "#EVALTRACE"

      -- All params for unwrapping
      allParams = givenParamInfo <> assumeParamInfo

      -- Separate boolean and non-boolean params (for unwrapping logic)
      nonBoolAllParams = filter (\(_, isB, _, _) -> not isB) allParams

      -- Generate argument expressions for GIVEN params only (in original order)
      -- Booleans use fromMaybe FALSE directly
      -- Non-booleans reference unwrapped variable names
      -- Date types need conversion via dateConvertedVar
      givenArgExprs = map snd $ sortOn fst $
        [(idx, expr) | (idx, ((name, _), True, _, _)) <- zip [0 :: Int ..] givenParamInfo
                     , let expr = "(fromMaybe FALSE (args's " <> quoteIdent name <> "))"] ++
        [(idx, expr) | (idx, ((name, _), False, _, isDate)) <- zip [0 :: Int ..] givenParamInfo
                     , let expr = if isDate then dateConvertedVar name else unwrappedVar name]

      -- Build function call with only GIVEN params as arguments
      -- Quote function name if it contains spaces
      quotedFunName = quoteIdent funName
      functionCall = if null givenArgExprs
        then quotedFunName
        else quotedFunName <> " " <> Text.unwords givenArgExprs

      -- Wrap function call with LET bindings for ASSUME params
      -- Boolean ASSUMEs use fromMaybe FALSE
      -- Non-boolean ASSUMEs use unwrapped_ variables
      -- Date ASSUMEs use dateConverted_ variables
      wrapWithAssumes :: Text -> Text
      wrapWithAssumes innerExpr = foldr wrapOne innerExpr assumeParamInfo
        where
          wrapOne ((name, _), isBoolean, _, isDate) expr =
            let quotedName = quoteIdent name
                valueExpr = if isBoolean
                  then "(fromMaybe FALSE (args's " <> quotedName <> "))"
                  else if isDate then dateConvertedVar name else unwrappedVar name
            in "LET " <> quotedName <> " = " <> valueExpr <> " IN " <> expr

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
-- For DATE types, adds an additional CONSIDER to convert string -> DATE via TODATE.
generateNestedConsiderWithAssumes
  :: [((Text, Type' Resolved), Bool, Bool, Bool)]  -- ^ Non-boolean params to unwrap (name, type, isBoolean, isGiven, isDate)
  -> Text                                           -- ^ Function call (GIVEN args only)
  -> [((Text, Type' Resolved), Bool, Bool, Bool)]  -- ^ ASSUME params for LET bindings
  -> Int                                            -- ^ Indentation level
  -> [Text]
generateNestedConsiderWithAssumes [] functionCall assumeParamInfo indent =
  let indentStr = Text.replicate indent "  "
      -- Wrap function call with LET bindings for ASSUME params
      wrapWithAssumes :: Text -> Text
      wrapWithAssumes innerExpr = foldr wrapOne innerExpr assumeParamInfo
        where
          wrapOne ((name, _), isBoolean, _, isDate) expr =
            let quotedName = quoteIdent name
                valueExpr = if isBoolean
                  then "(fromMaybe FALSE (args's " <> quotedName <> "))"
                  else if isDate then dateConvertedVar name else unwrappedVar name
            in "LET " <> quotedName <> " = " <> valueExpr <> " IN " <> expr
  in [indentStr <> "JUST (" <> wrapWithAssumes functionCall <> ")"]
generateNestedConsiderWithAssumes (((name, _), _, _, isDate):rest) functionCall assumeParamInfo indent =
  let indentStr = Text.replicate indent "  "
      quotedName = quoteIdent name
  in if isDate
     then
       -- For DATE types: first unwrap the MAYBE STRING, then convert to DATE via TODATE
       [ indentStr <> "CONSIDER args's " <> quotedName
       , indentStr <> "  WHEN JUST " <> unwrappedVar name <> " THEN"
       , indentStr <> "    CONSIDER TODATE " <> unwrappedVar name
       , indentStr <> "      WHEN JUST " <> dateConvertedVar name <> " THEN"
       ] ++
       generateNestedConsiderWithAssumes rest functionCall assumeParamInfo (indent + 4) ++
       [ indentStr <> "      WHEN NOTHING THEN NOTHING  -- date parse failed"
       , indentStr <> "  WHEN NOTHING THEN NOTHING"
       ]
     else
       -- For non-DATE types: just unwrap the MAYBE
       [ indentStr <> "CONSIDER args's " <> quotedName
       , indentStr <> "  WHEN JUST " <> unwrappedVar name <> " THEN"
       ] ++
       generateNestedConsiderWithAssumes rest functionCall assumeParamInfo (indent + 2) ++
       [ indentStr <> "  WHEN NOTHING THEN NOTHING"
       ]

-- | Create a variable name for date-converted values
-- For names with spaces, creates a backtick-quoted identifier like `dateConverted_earn date`
dateConvertedVar :: Text -> Text
dateConvertedVar name
  | Text.any (== ' ') name = "`dateConverted_" <> name <> "`"
  | otherwise = "dateConverted_" <> name
