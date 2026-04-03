{-# LANGUAGE OverloadedStrings #-}
module Backend.CodeGen
  ( generateEvalWrapper
  , GeneratedCode(..)
  -- Exported for testing
  , inputFieldName
  , transformJsonKeys
  ) where

import Base
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AesonKeyMap
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import L4.Syntax (Type'(..), Resolved, getUnique)
import L4.TypeCheck.Environment (booleanUnique, dateUnique, timeUnique, datetimeUnique)
import Backend.Api (TraceLevel(..))
import Backend.MaybeLift (liftTypeToMaybe)

-- | Determine if a type needs string->value conversion from JSON input.
-- Returns the conversion function name (e.g. "TODATE", "TOTIME", "TODATETIME")
-- or Nothing if no conversion is needed.
stringConversionFn :: Type' Resolved -> Maybe Text
stringConversionFn ty = coreConversion (unwrapMaybe ty)
  where
    coreConversion (TyApp _ name [])
      | getUnique name == dateUnique     = Just "TODATE"
      | getUnique name == timeUnique     = Just "TOTIME"
      | getUnique name == datetimeUnique = Just "TODATETIME"
    coreConversion _ = Nothing
    unwrapMaybe :: Type' Resolved -> Type' Resolved
    unwrapMaybe (TyApp _ _ [inner]) = unwrapMaybe inner
    unwrapMaybe t = t

-- | Quote an identifier with backticks if it contains spaces or special characters
-- L4 requires backticks for identifiers with spaces
quoteIdent :: Text -> Text
quoteIdent name
  | Text.any (== ' ') name = "`" <> name <> "`"
  | otherwise = name

-- | Prefix for InputArgs field names to avoid collision with source identifiers.
-- The generated InputArgs record creates accessor functions, which would otherwise
-- collide with GIVEN parameter names in the original source file.
-- We use a suffix approach: "name" -> "name (input)" to keep field names readable
-- while avoiding conflicts.
inputFieldName :: Text -> Text
inputFieldName name = name <> " (input)"

-- | Quote an InputArgs field name (which has been prefixed)
quoteInputField :: Text -> Text
quoteInputField = quoteIdent . inputFieldName

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
          -- Annotate GIVEN params with (type info, isBoolean, isGiven, convFn)
          givenParamInfo = map (\(name, ty) -> ((name, ty), isBooleanType ty, True, stringConversionFn ty)) givenParams
          -- Annotate ASSUME params with (type info, isBoolean, isGiven, convFn)
          assumeParamInfo = map (\(name, ty) -> ((name, ty), isBooleanType ty, False, stringConversionFn ty)) assumeParams
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
          -- Use prefixed field names to avoid collision with source identifiers
          quotedFieldName = quoteInputField name
      in indent <> quotedFieldName <> " IS A " <> tyText

-- | Generate typed decoder function
generateDecoder :: Text
generateDecoder = Text.unlines
  [ "GIVEN jsn IS A STRING"
  , "GIVETH AN EITHER STRING InputArgs"
  , "decodeArgs jsn MEANS JSONDECODE jsn"
  ]

-- | Generate JSON payload as L4 string literal
-- Transforms JSON keys to match the renamed InputArgs field names
generateJsonPayload :: Aeson.Value -> Text
generateJsonPayload json =
  "DECIDE inputJson IS " <> escapeAsL4String (transformJsonKeys json)

-- | Transform JSON object keys to match InputArgs field names
-- Adds " (input)" suffix to all top-level keys to match the collision-avoiding
-- field names in the generated InputArgs record.
transformJsonKeys :: Aeson.Value -> Aeson.Value
transformJsonKeys (Aeson.Object obj) =
  Aeson.Object $ AesonKeyMap.fromList $ map transformPair $ AesonKeyMap.toList obj
  where
    transformPair (key, val) =
      (AesonKey.fromText (inputFieldName (AesonKey.toText key)), val)
transformJsonKeys other = other

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
-- Type signature: [((name, type), isBoolean, isGiven, convFn)]
-- convFn: Just "TODATE"/"TOTIME"/"TODATETIME" if type needs string->value conversion, Nothing otherwise
generateEvalDirectiveLiftedWithAssumes
  :: Text
  -> [((Text, Type' Resolved), Bool, Bool, Maybe Text)]  -- ^ GIVEN params: ((name, type), isBoolean, isGiven=True, convFn)
  -> [((Text, Type' Resolved), Bool, Bool, Maybe Text)]  -- ^ ASSUME params: ((name, type), isBoolean, isGiven=False, convFn)
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
      givenArgExprs = map snd $ sortOn fst $
        [(idx, expr) | (idx, ((name, _), True, _, _)) <- zip [0 :: Int ..] givenParamInfo
                     , let expr = "(fromMaybe FALSE (args's " <> quoteInputField name <> "))"] ++
        [(idx, expr) | (idx, ((name, _), False, _, convFn)) <- zip [0 :: Int ..] givenParamInfo
                     , let expr = case convFn of
                             Just _ -> convertedVar name
                             Nothing -> unwrappedVar name]

      -- Build function call with only GIVEN params as arguments
      quotedFunName = quoteIdent funName
      functionCall = if null givenArgExprs
        then quotedFunName
        else quotedFunName <> " " <> Text.unwords givenArgExprs

      -- Wrap function call with LET bindings for ASSUME params
      wrapWithAssumes :: Text -> Text
      wrapWithAssumes innerExpr = foldr wrapOne innerExpr assumeParamInfo
        where
          wrapOne ((name, _), isBoolean, _, convFn) expr =
            let quotedName = quoteIdent name
                valueExpr
                  | isBoolean = "(fromMaybe FALSE (args's " <> quoteInputField name <> "))"
                  | Just _ <- convFn = convertedVar name
                  | otherwise = unwrappedVar name
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
-- For types needing conversion (DATE/TIME/DATETIME), adds an additional CONSIDER
-- to convert string -> typed value via TODATE/TOTIME/TODATETIME.
generateNestedConsiderWithAssumes
  :: [((Text, Type' Resolved), Bool, Bool, Maybe Text)]  -- ^ Non-boolean params to unwrap
  -> Text                                                  -- ^ Function call (GIVEN args only)
  -> [((Text, Type' Resolved), Bool, Bool, Maybe Text)]  -- ^ ASSUME params for LET bindings
  -> Int                                                   -- ^ Indentation level
  -> [Text]
generateNestedConsiderWithAssumes [] functionCall assumeParamInfo indent =
  let indentStr = Text.replicate indent "  "
      -- Wrap function call with LET bindings for ASSUME params
      wrapWithAssumes :: Text -> Text
      wrapWithAssumes innerExpr = foldr wrapOne innerExpr assumeParamInfo
        where
          wrapOne ((name, _), isBoolean, _, convFn) expr =
            let quotedName = quoteIdent name
                valueExpr
                  | isBoolean = "(fromMaybe FALSE (args's " <> quoteInputField name <> "))"
                  | Just _ <- convFn = convertedVar name
                  | otherwise = unwrappedVar name
            in "LET " <> quotedName <> " = " <> valueExpr <> " IN " <> expr
  in [indentStr <> "JUST (" <> wrapWithAssumes functionCall <> ")"]
generateNestedConsiderWithAssumes (((name, _), _, _, convFn):rest) functionCall assumeParamInfo indent =
  let indentStr = Text.replicate indent "  "
      quotedFieldName = quoteInputField name
  in case convFn of
     Just fn ->
       -- Type needs string->value conversion: unwrap MAYBE STRING, then convert
       [ indentStr <> "CONSIDER args's " <> quotedFieldName
       , indentStr <> "  WHEN JUST " <> unwrappedVar name <> " THEN"
       , indentStr <> "    CONSIDER " <> fn <> " " <> unwrappedVar name
       , indentStr <> "      WHEN JUST " <> convertedVar name <> " THEN"
       ] ++
       generateNestedConsiderWithAssumes rest functionCall assumeParamInfo (indent + 4) ++
       [ indentStr <> "      WHEN NOTHING THEN NOTHING  -- parse failed"
       , indentStr <> "  WHEN NOTHING THEN NOTHING"
       ]
     Nothing ->
       -- No conversion needed: just unwrap the MAYBE
       [ indentStr <> "CONSIDER args's " <> quotedFieldName
       , indentStr <> "  WHEN JUST " <> unwrappedVar name <> " THEN"
       ] ++
       generateNestedConsiderWithAssumes rest functionCall assumeParamInfo (indent + 2) ++
       [ indentStr <> "  WHEN NOTHING THEN NOTHING"
       ]

-- | Create a variable name for converted values (DATE/TIME/DATETIME from string)
convertedVar :: Text -> Text
convertedVar name
  | Text.any (== ' ') name = "`converted_" <> name <> "`"
  | otherwise = "converted_" <> name
