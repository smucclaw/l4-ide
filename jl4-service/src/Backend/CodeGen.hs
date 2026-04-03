{-# LANGUAGE OverloadedStrings #-}
module Backend.CodeGen
  ( generateEvalWrapper
  , generateDeonticEvalWrapper
  , isDeonticType
  , GeneratedCode(..)
  -- Exported for testing
  , inputFieldName
  , transformJsonKeys
  , fnLiteralToL4Expr
  , fnLiteralToL4ExprWithType
  ) where

import Base
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AesonKeyMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import L4.Syntax (Type'(..), Resolved, getUnique)
import L4.TypeCheck.Environment (booleanUnique, dateUnique, timeUnique, datetimeUnique, maybeUnique, contractUnique)
import Backend.Api (TraceLevel(..), TraceEvent(..), FnLiteral(..))
import qualified Base.Text as BaseText
import Backend.MaybeLift (liftTypeToMaybe)
import qualified Data.Scientific as Scientific

-- | Determine if a type needs string→value conversion from JSON input.
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
    -- Unwrap MAYBE layers to get to the core type
    unwrapMaybe :: Type' Resolved -> Type' Resolved
    unwrapMaybe (TyApp _ _ [inner]) = unwrapMaybe inner
    unwrapMaybe t = t

-- | Check if a type is MAYBE X (one level)
isMaybeType :: Type' Resolved -> Bool
isMaybeType (TyApp _ name [_]) = getUnique name == maybeUnique
isMaybeType _ = False

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
          -- Annotate GIVEN params with (type info, isBoolean, isGiven, convFn, isMaybe)
          givenParamInfo = map (\(name, ty) -> ((name, ty), isBooleanType ty, True, stringConversionFn ty, isMaybeType ty)) givenParams
          -- Annotate ASSUME params with (type info, isBoolean, isGiven, convFn, isMaybe)
          assumeParamInfo = map (\(name, ty) -> ((name, ty), isBooleanType ty, False, stringConversionFn ty, isMaybeType ty)) assumeParams
          allParamInfo = givenParamInfo <> assumeParamInfo
          -- We always need prelude for fromMaybe (all booleans use it)
          hasBooleans = any (\(_, isB, _, _, _) -> isB) allParamInfo
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
-- Type signature: [((name, type), isBoolean, isGiven, convFn, isMaybe)]
-- convFn: Just "TODATE"/"TOTIME"/"TODATETIME" if type needs string->value conversion, Nothing otherwise
-- isMaybe=True means the parameter's original type is already MAYBE X, so we
-- should NOT unwrap the value (pass it directly as MAYBE X to the function).
generateEvalDirectiveLiftedWithAssumes
  :: Text
  -> [((Text, Type' Resolved), Bool, Bool, Maybe Text, Bool)]  -- ^ GIVEN params: ((name, type), isBoolean, isGiven=True, convFn, isMaybe)
  -> [((Text, Type' Resolved), Bool, Bool, Maybe Text, Bool)]  -- ^ ASSUME params: ((name, type), isBoolean, isGiven=False, convFn, isMaybe)
  -> TraceLevel
  -> Text
generateEvalDirectiveLiftedWithAssumes funName givenParamInfo assumeParamInfo traceLevel =
  let directive = case traceLevel of
        TraceNone -> "#EVAL"
        TraceFull -> "#EVALTRACE"

      -- All params for unwrapping
      allParams = givenParamInfo <> assumeParamInfo

      -- Params that need CONSIDER unwrapping:
      -- non-boolean AND not originally-MAYBE (isMaybe=False)
      needsUnwrap (_, isB, _, _, isM) = not isB && not isM
      nonBoolNonMaybeParams = filter needsUnwrap allParams

      -- Generate the value expression for a single parameter
      paramValueExpr :: (Text, Bool, Maybe Text, Bool) -> Text
      paramValueExpr (name, isBoolean, convFn, isM)
        | isBoolean = "(fromMaybe FALSE (args's " <> quoteInputField name <> "))"
        -- Originally MAYBE: pass field value directly (it's already MAYBE X)
        | isM, Just _ <- convFn = maybeConvertedVar name
        | isM       = "(args's " <> quoteInputField name <> ")"
        | Just _ <- convFn = convertedVar name
        | otherwise = unwrappedVar name

      -- Generate argument expressions for GIVEN params only (in original order)
      givenArgExprs = map snd $ sortOn fst $
        [(idx, paramValueExpr (name, isB, convFn, isM))
        | (idx, ((name, _), isB, _, convFn, isM)) <- zip [0 :: Int ..] givenParamInfo]

      -- Build function call with only GIVEN params as arguments
      -- Quote function name if it contains spaces
      quotedFunName = quoteIdent funName
      functionCall = if null givenArgExprs
        then quotedFunName
        else quotedFunName <> " " <> Text.unwords givenArgExprs

      -- Wrap function call with LET bindings for ASSUME params
      wrapWithAssumes :: Text -> Text
      wrapWithAssumes innerExpr = foldr wrapOne innerExpr assumeParamInfo
        where
          wrapOne ((name, _), isB, _, convFn, isM) expr =
            let quotedName = quoteIdent name
                valueExpr = paramValueExpr (name, isB, convFn, isM)
            in "LET " <> quotedName <> " = " <> valueExpr <> " IN " <> expr

      -- The innermost expression (wrapped function call in JUST)
      innerCall = "JUST (" <> wrapWithAssumes functionCall <> ")"

      -- Generate LET bindings for MAYBE params that need string->value conversion
      -- (e.g. MAYBE DATE, MAYBE TIME, MAYBE DATETIME)
      -- These are placed before the nested CONSIDER but after the decode
      maybeConvParams = filter isMaybeConv allParams
        where isMaybeConv (_, _, _, Just _, True) = True
              isMaybeConv _ = False
      maybeConvLetBindings = concatMap genMaybeConvLet maybeConvParams
      genMaybeConvLet ((name, _), _, _, Just fn, _) =
        [ "      LET " <> maybeConvertedVar name <> " ="
        , "        CONSIDER args's " <> quoteInputField name
        , "          WHEN JUST " <> unwrappedVar name <> " THEN " <> fn <> " " <> unwrappedVar name
        , "          WHEN NOTHING THEN NOTHING"
        , "      IN"
        ]
      genMaybeConvLet _ = []

  in if null nonBoolNonMaybeParams && null maybeConvParams
     then -- All booleans/passthrough: simple case, no nested CONSIDER needed
       Text.unlines
         [ directive
         , "  CONSIDER decodeArgs inputJson"
         , "    WHEN RIGHT args THEN " <> innerCall
         , "    WHEN LEFT error THEN NOTHING"
         ]
     else if null nonBoolNonMaybeParams
     then -- Only MAYBE conversion params need LET bindings, no CONSIDER unwrapping
       Text.unlines $
         [ directive
         , "  CONSIDER decodeArgs inputJson"
         , "    WHEN RIGHT args THEN"
         ] ++
         maybeConvLetBindings ++
         [ "      " <> innerCall
         , "    WHEN LEFT error THEN NOTHING"
         ]
     else -- Has params that need CONSIDER unwrapping
       Text.unlines $
         [ directive
         , "  CONSIDER decodeArgs inputJson"
         , "    WHEN RIGHT args THEN"
         ] ++
         maybeConvLetBindings ++
         generateNestedConsiderWithAssumes nonBoolNonMaybeParams functionCall assumeParamInfo (3 + if null maybeConvParams then 0 else 1) ++
         [ "    WHEN LEFT error THEN NOTHING"
         ]

-- | Generate nested CONSIDER for unwrapping non-boolean, non-MAYBE values,
-- with LET bindings for ASSUME params at the innermost level.
-- For types needing conversion (DATE/TIME/DATETIME), adds an additional CONSIDER
-- to convert string -> typed value via TODATE/TOTIME/TODATETIME.
generateNestedConsiderWithAssumes
  :: [((Text, Type' Resolved), Bool, Bool, Maybe Text, Bool)]  -- ^ Params to unwrap (non-boolean, non-MAYBE)
  -> Text                                                       -- ^ Function call (GIVEN args only)
  -> [((Text, Type' Resolved), Bool, Bool, Maybe Text, Bool)]  -- ^ ASSUME params for LET bindings
  -> Int                                                        -- ^ Indentation level
  -> [Text]
generateNestedConsiderWithAssumes [] functionCall assumeParamInfo indent =
  let indentStr = Text.replicate indent "  "
      -- Wrap function call with LET bindings for ASSUME params
      wrapWithAssumes :: Text -> Text
      wrapWithAssumes innerExpr = foldr wrapOne innerExpr assumeParamInfo
        where
          wrapOne ((name, _), isB, _, convFn, isM) expr =
            let quotedName = quoteIdent name
                valueExpr
                  | isB       = "(fromMaybe FALSE (args's " <> quoteInputField name <> "))"
                  | isM, Just _ <- convFn = maybeConvertedVar name
                  | isM       = "(args's " <> quoteInputField name <> ")"
                  | Just _ <- convFn = convertedVar name
                  | otherwise = unwrappedVar name
            in "LET " <> quotedName <> " = " <> valueExpr <> " IN " <> expr
  in [indentStr <> "JUST (" <> wrapWithAssumes functionCall <> ")"]
generateNestedConsiderWithAssumes (((name, _), _, _, convFn, _):rest) functionCall assumeParamInfo indent =
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

-- | Create a variable name for MAYBE converted values
-- Used when the original type is MAYBE DATE/TIME/DATETIME (string->value conversion preserving MAYBE)
maybeConvertedVar :: Text -> Text
maybeConvertedVar name
  | Text.any (== ' ') name = "`maybeConverted_" <> name <> "`"
  | otherwise = "maybeConverted_" <> name

-- ----------------------------------------------------------------------------
-- Deontic evaluation support
-- ----------------------------------------------------------------------------

-- | Check if a type is DEONTIC a b (contract type with two type parameters)
isDeonticType :: Type' Resolved -> Bool
isDeonticType (TyApp _ name [_, _]) = getUnique name == contractUnique
isDeonticType _ = False

-- | Convert an FnLiteral to inline L4 source text.
-- Used for generating EVENT expressions in deontic wrappers.
fnLiteralToL4Expr :: FnLiteral -> Text
fnLiteralToL4Expr = \case
  FnLitString s -> "`" <> s <> "`"
  FnLitInt i -> BaseText.textShow i
  FnLitDouble d -> BaseText.textShow d
  FnLitBool True -> "TRUE"
  FnLitBool False -> "FALSE"
  FnArray xs -> "(LIST " <> Text.intercalate ", " (map fnLiteralToL4Expr xs) <> ")"
  FnObject [(conName, FnObject fields)] ->
    -- Record constructor: `Constructor` WITH field1 IS val1, field2 IS val2
    quoteIdent conName <> " WITH " <>
      Text.intercalate ", " [quoteIdent k <> " IS " <> fnLiteralToL4Expr v | (k, v) <- fields]
  FnObject [(conName, FnArray [])] ->
    -- Nullary constructor
    quoteIdent conName
  FnObject [(conName, FnArray [singleVal])] ->
    -- Unary constructor with positional arg
    quoteIdent conName <> " " <> fnLiteralToL4Expr singleVal
  FnObject [("tag", val)] ->
    -- Tagged union encoding: {"tag": "ConstructorName"} → just the value
    fnLiteralToL4Expr val
  FnObject [(conName, val)] ->
    -- Single-key object: constructor applied to argument
    quoteIdent conName <> " " <> fnLiteralToL4Expr val
  FnObject _ -> "NOTHING"  -- Fallback for unexpected shapes
  FnUncertain -> "NOTHING"
  FnUnknown -> "NOTHING"

-- | Convert FnLiteral to L4 source, using the type name to wrap record values.
-- The type name is only needed for record types (bare FnObject with multiple fields).
-- For enums (FnLitString) and tagged unions (FnObject with single key), the existing
-- fnLiteralToL4Expr already produces correct L4 because the constructor info is in the value.
fnLiteralToL4ExprWithType :: Maybe Text -> FnLiteral -> Text
fnLiteralToL4ExprWithType Nothing lit = fnLiteralToL4Expr lit
fnLiteralToL4ExprWithType (Just typeName) lit = case lit of
  -- FnObject with any fields → record type, needs constructor wrapper
  -- {"name": "Alice"} → Driver WITH name IS "Alice"
  -- {"name": "Alice", "age": 30} → Driver WITH name IS "Alice", age IS 30
  -- Note: field VALUES use fnLiteralToL4Value (strings as "quoted literals")
  -- rather than fnLiteralToL4Expr (strings as `backtick identifiers`).
  -- This is critical because record field values are data, not constructor references.
  FnObject fields@(_:_) ->
    quoteIdent typeName <> " WITH " <>
      Text.intercalate ", " [quoteIdent k <> " IS " <> fnLiteralToL4Value v | (k, v) <- fields]
  -- FnLitString → enum constructor (existing logic handles this)
  -- Everything else → delegate to existing function
  other -> fnLiteralToL4Expr other

-- | Convert FnLiteral to L4 source text as a data value.
-- Unlike fnLiteralToL4Expr (which treats strings as constructor references
-- using backtick quoting), this formats strings as double-quoted string literals.
-- Used for record field values where strings represent data, not constructors.
fnLiteralToL4Value :: FnLiteral -> Text
fnLiteralToL4Value = \case
  FnLitString s -> "\"" <> escapeL4String s <> "\""
  FnArray xs -> "(LIST " <> Text.intercalate ", " (map fnLiteralToL4Value xs) <> ")"
  other -> fnLiteralToL4Expr other
  where
    escapeL4String = Text.replace "\\" "\\\\" . Text.replace "\"" "\\\""

-- | Prepare events for EVALTRACE by generating MEANS definitions for record-typed
-- party/action values and a formatted event list expression.
--
-- Record values (FnObject) can't be inlined in EVENT expressions because L4's parser
-- doesn't handle WITH keyword inside parenthesized arguments. Instead, we generate
-- top-level MEANS definitions and reference them by name — matching how L4 source
-- code typically defines named constants for record values.
--
-- Example: {"name": "Alice"} with party type "Driver" generates:
--   `event party 0` MEANS Driver WITH name IS "Alice"
--   ... EVENT (`event party 0`) (`wear seatbelt`) 1
prepareEvents
  :: Maybe Text -> Maybe Text -> [TraceEvent]
  -> ([Text], Text)  -- ^ (MEANS definitions, event list expression)
prepareEvents mPartyType mActionType events =
  let isRecordLit (FnObject (_:_)) = True
      isRecordLit _ = False

      -- Deduplicated record-typed party values → binding names
      uniquePartyRecords :: Map FnLiteral Text
      uniquePartyRecords = Map.fromList
        [ (lit, "event party " <> BaseText.textShow i)
        | (i, lit) <- zip [0 :: Int ..]
            $ Set.toList $ Set.fromList [ev.party | ev <- events, isRecordLit ev.party]
        ]

      uniqueActionRecords :: Map FnLiteral Text
      uniqueActionRecords = Map.fromList
        [ (lit, "event action " <> BaseText.textShow i)
        | (i, lit) <- zip [0 :: Int ..]
            $ Set.toList $ Set.fromList [ev.action | ev <- events, isRecordLit ev.action]
        ]

      -- MEANS definitions for record values
      meansDefns =
        [ quoteIdent name <> " MEANS " <> fnLiteralToL4ExprWithType mPartyType lit
        | (lit, name) <- Map.toList uniquePartyRecords
        ] ++
        [ quoteIdent name <> " MEANS " <> fnLiteralToL4ExprWithType mActionType lit
        | (lit, name) <- Map.toList uniqueActionRecords
        ]

      -- Format a single event, using binding names for records
      fmtEvent ev =
        let partyExpr = case Map.lookup ev.party uniquePartyRecords of
              Just name -> quoteIdent name
              Nothing   -> fnLiteralToL4ExprWithType mPartyType ev.party
            actionExpr = case Map.lookup ev.action uniqueActionRecords of
              Just name -> quoteIdent name
              Nothing   -> fnLiteralToL4ExprWithType mActionType ev.action
        in "EVENT (" <> partyExpr <> ") (" <> actionExpr <> ") " <> formatScientific ev.at

      eventExprs = map fmtEvent events
      eventListExpr = "(LIST " <> Text.intercalate ", " eventExprs <> ")"
  in (meansDefns, eventListExpr)

-- | Format a Scientific number as L4 source
formatScientific :: Scientific.Scientific -> Text
formatScientific n
  | Scientific.isInteger n = BaseText.textShow (Scientific.coefficient n * (10 ^ Scientific.base10Exponent n))
  | otherwise = BaseText.textShow (Scientific.toRealFloat n :: Double)

-- | Generate L4 wrapper code for deontic evaluation with EVALTRACE.
-- Extends generateEvalWrapper by wrapping the function call in EVALTRACE.
generateDeonticEvalWrapper
  :: Text                         -- ^ Target function name
  -> [(Text, Type' Resolved)]     -- ^ GIVEN parameter names and types
  -> [(Text, Type' Resolved)]     -- ^ ASSUME parameter names and types
  -> Aeson.Value                  -- ^ Input arguments as JSON object
  -> Scientific.Scientific        -- ^ Start time
  -> [TraceEvent]                 -- ^ Events (may be empty for initial state)
  -> Maybe Text                   -- ^ Party type name (for formatting events)
  -> Maybe Text                   -- ^ Action type name (for formatting events)
  -> TraceLevel                   -- ^ Whether to generate EVAL or EVALTRACE
  -> Either Text GeneratedCode
generateDeonticEvalWrapper funName givenParams assumeParams inputJson startTime events mPartyType mActionType traceLevel = do
  let allParams = givenParams <> assumeParams

  -- Build event list expression with MEANS bindings for record-typed values
  let (recordMeansDefns, eventListExpr) = prepareEvents mPartyType mActionType events
      startTimeExpr = formatScientific startTime

  -- Handle zero-parameter functions: wrap direct call in EVALTRACE
  if null allParams
    then Right GeneratedCode
      { generatedWrapper = Text.unlines $
          [ ""
          , "-- ========== GENERATED DEONTIC WRAPPER =========="
          ] ++
          recordMeansDefns ++
          [ ""
          , generateSimpleDeonticEval funName startTimeExpr eventListExpr traceLevel
          ]
      , decodeFailedSentinel = "DECODE_FAILED"
      }
    else
      -- Deep Maybe lifting: all parameters get MAYBE types (same as generateEvalWrapper)
      let isBooleanType :: Type' Resolved -> Bool
          isBooleanType (TyApp _ name []) = getUnique name == booleanUnique
          isBooleanType _ = False
          givenParamInfo = map (\(name, ty) -> ((name, ty), isBooleanType ty, True, stringConversionFn ty, isMaybeType ty)) givenParams
          assumeParamInfo = map (\(name, ty) -> ((name, ty), isBooleanType ty, False, stringConversionFn ty, isMaybeType ty)) assumeParams
          allParamInfo = givenParamInfo <> assumeParamInfo
          hasBooleans = any (\(_, isB, _, _, _) -> isB) allParamInfo
      in Right GeneratedCode
      { generatedWrapper = Text.unlines $
          [ ""
          , "-- ========== GENERATED DEONTIC WRAPPER (Deep Maybe Lifting) =========="
          ] ++
          (if hasBooleans then ["IMPORT prelude  -- for fromMaybe"] else []) ++
          [ ""
          , generateInputRecordLifted allParams
          , ""
          , generateDecoder
          , ""
          , generateJsonPayload inputJson
          ] ++
          (if null recordMeansDefns then [] else "" : recordMeansDefns) ++
          [ ""
          , generateDeonticEvalDirectiveLifted funName givenParamInfo assumeParamInfo startTimeExpr eventListExpr traceLevel
          ]
      , decodeFailedSentinel = "DECODE_FAILED"
      }

-- | Generate simple EVALTRACE for zero-parameter deontic functions
generateSimpleDeonticEval :: Text -> Text -> Text -> TraceLevel -> Text
generateSimpleDeonticEval funName startTimeExpr eventListExpr traceLevel =
  let directive = case traceLevel of
        TraceNone -> "#EVAL"
        TraceFull -> "#EVALTRACE"
  in directive <> " EVALTRACE (" <> quoteIdent funName <> ") " <> startTimeExpr <> " " <> eventListExpr

-- | Generate EVAL/EVALTRACE with EVALTRACE call wrapping the function, with parameter handling
generateDeonticEvalDirectiveLifted
  :: Text
  -> [((Text, Type' Resolved), Bool, Bool, Maybe Text, Bool)]  -- ^ GIVEN params
  -> [((Text, Type' Resolved), Bool, Bool, Maybe Text, Bool)]  -- ^ ASSUME params
  -> Text                                                       -- ^ Start time expression
  -> Text                                                       -- ^ Event list expression
  -> TraceLevel
  -> Text
generateDeonticEvalDirectiveLifted funName givenParamInfo assumeParamInfo startTimeExpr eventListExpr traceLevel =
  let directive = case traceLevel of
        TraceNone -> "#EVAL"
        TraceFull -> "#EVALTRACE"

      allParams = givenParamInfo <> assumeParamInfo

      needsUnwrap (_, isB, _, _, isM) = not isB && not isM
      nonBoolNonMaybeParams = filter needsUnwrap allParams

      paramValueExpr :: (Text, Bool, Maybe Text, Bool) -> Text
      paramValueExpr (name, isBoolean, convFn, isM)
        | isBoolean = "(fromMaybe FALSE (args's " <> quoteInputField name <> "))"
        | isM, Just _ <- convFn = maybeConvertedVar name
        | isM       = "(args's " <> quoteInputField name <> ")"
        | Just _ <- convFn = convertedVar name
        | otherwise = unwrappedVar name

      givenArgExprs = map snd $ sortOn fst $
        [(idx, paramValueExpr (name, isB, convFn, isM))
        | (idx, ((name, _), isB, _, convFn, isM)) <- zip [0 :: Int ..] givenParamInfo]

      quotedFunName = quoteIdent funName
      functionCall = if null givenArgExprs
        then quotedFunName
        else quotedFunName <> " " <> Text.unwords givenArgExprs

      -- Wrap function call with LET bindings for ASSUME params
      wrapWithAssumes :: Text -> Text
      wrapWithAssumes innerExpr = foldr wrapOne innerExpr assumeParamInfo
        where
          wrapOne ((name, _), isB, _, convFn, isM) expr =
            let quotedName = quoteIdent name
                valueExpr = paramValueExpr (name, isB, convFn, isM)
            in "LET " <> quotedName <> " = " <> valueExpr <> " IN " <> expr

      -- Wrap the function call in EVALTRACE
      evalTraceCall = "EVALTRACE (" <> wrapWithAssumes functionCall <> ") " <> startTimeExpr <> " " <> eventListExpr
      innerCall = "JUST (" <> evalTraceCall <> ")"

      -- LET bindings for MAYBE params that need string->value conversion
      maybeConvParams = filter isMaybeConv allParams
        where isMaybeConv (_, _, _, Just _, True) = True
              isMaybeConv _ = False
      maybeConvLetBindings = concatMap genMaybeConvLet maybeConvParams
      genMaybeConvLet ((name, _), _, _, Just fn, _) =
        [ "      LET " <> maybeConvertedVar name <> " ="
        , "        CONSIDER args's " <> quoteInputField name
        , "          WHEN JUST " <> unwrappedVar name <> " THEN " <> fn <> " " <> unwrappedVar name
        , "          WHEN NOTHING THEN NOTHING"
        , "      IN"
        ]
      genMaybeConvLet _ = []

  in if null nonBoolNonMaybeParams && null maybeConvParams
     then Text.unlines
       [ directive
       , "  CONSIDER decodeArgs inputJson"
       , "    WHEN RIGHT args THEN " <> innerCall
       , "    WHEN LEFT error THEN NOTHING"
       ]
     else if null nonBoolNonMaybeParams
     then Text.unlines $
       [ directive
       , "  CONSIDER decodeArgs inputJson"
       , "    WHEN RIGHT args THEN"
       ] ++
       maybeConvLetBindings ++
       [ "      " <> innerCall
       , "    WHEN LEFT error THEN NOTHING"
       ]
     else Text.unlines $
       [ directive
       , "  CONSIDER decodeArgs inputJson"
       , "    WHEN RIGHT args THEN"
       ] ++
       maybeConvLetBindings ++
       generateNestedConsiderDeontic nonBoolNonMaybeParams evalTraceCall assumeParamInfo (3 + if null maybeConvParams then 0 else 1) ++
       [ "    WHEN LEFT error THEN NOTHING"
       ]

-- | Generate nested CONSIDER for deontic evaluation (wraps result in EVALTRACE)
generateNestedConsiderDeontic
  :: [((Text, Type' Resolved), Bool, Bool, Maybe Text, Bool)]  -- ^ Params to unwrap
  -> Text                                                       -- ^ EVALTRACE function call
  -> [((Text, Type' Resolved), Bool, Bool, Maybe Text, Bool)]  -- ^ ASSUME params for LET bindings
  -> Int                                                        -- ^ Indentation level
  -> [Text]
generateNestedConsiderDeontic [] evalTraceCall _assumeParamInfo indent =
  let indentStr = Text.replicate indent "  "
  in [indentStr <> "JUST (" <> evalTraceCall <> ")"]
generateNestedConsiderDeontic (((name, _), _, _, convFn, _):rest) evalTraceCall assumeParamInfo indent =
  let indentStr = Text.replicate indent "  "
      quotedFieldName = quoteInputField name
  in case convFn of
     Just fn ->
       [ indentStr <> "CONSIDER args's " <> quotedFieldName
       , indentStr <> "  WHEN JUST " <> unwrappedVar name <> " THEN"
       , indentStr <> "    CONSIDER " <> fn <> " " <> unwrappedVar name
       , indentStr <> "      WHEN JUST " <> convertedVar name <> " THEN"
       ] ++
       generateNestedConsiderDeontic rest evalTraceCall assumeParamInfo (indent + 4) ++
       [ indentStr <> "      WHEN NOTHING THEN NOTHING  -- parse failed"
       , indentStr <> "  WHEN NOTHING THEN NOTHING"
       ]
     Nothing ->
       [ indentStr <> "CONSIDER args's " <> quotedFieldName
       , indentStr <> "  WHEN JUST " <> unwrappedVar name <> " THEN"
       ] ++
       generateNestedConsiderDeontic rest evalTraceCall assumeParamInfo (indent + 2) ++
       [ indentStr <> "  WHEN NOTHING THEN NOTHING"
       ]
