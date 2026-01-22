{-# LANGUAGE CPP #-}
-- | Pure L4 API layer with optional JavaScript FFI exports.
--
-- This module provides pure Haskell functions for all L4 language features.
-- These functions are available for testing on all platforms.
-- When compiled for the WASM backend, they are also exported via JavaScript FFI.
--
-- The TypeScript bridge in @ts-apps/jl4-web/src/lib/wasm/wasm-bridge.ts@
-- calls these functions and expects JSON-encoded responses.
--
-- @since 0.1
module L4.API
  (
    -- * API Functions
    -- These functions are available for testing on all platforms.
    -- On WASM, they are also exported via JavaScript FFI.
    l4Check
  , l4Hover
  , l4Completions
  , l4SemanticTokens
  , l4Eval
  , l4VisualizeByName
  , l4CodeLenses
  , l4Definition
  , l4References
  ) where

import Base
import qualified Base.Text as Text
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LazyText
import qualified Data.Vector as Vector

import L4.Lexer (execLexer, PosToken(..), TokenType(..), TLiterals(..), TSpaces(..), TIdentifiers(..))

import L4.Annotation (HasSrcRange(..))
import L4.Parser.SrcSpan (SrcRange(..), SrcPos(..))
import L4.TypeCheck (severity, prettyCheckErrorWithContext, applyFinalSubstitution)
import L4.TypeCheck.Types (CheckResult(..), CheckErrorWithContext(..), Severity(..), Substitution)
import L4.Print (prettyLayout)
import L4.Syntax (Info(..), Type'(..), Resolved, OptionallyNamedType(..))
import L4.Annotation (emptyAnno)
import qualified L4.Utils.IntervalMap as IV
import qualified L4.EvaluateLazy as EL
import L4.EvaluateLazy.Machine (prettyEvalException)

import L4.TracePolicy (lspDefaultPolicy)
import L4.EvaluateLazy.GraphVizOptions (defaultGraphVizOptions)
import qualified L4.Viz.Ladder as Ladder
import L4.FindDefinition (findDefinition)
import L4.FindReferences (findReferences)

-- Import resolution (VFS + embedded libs)
import L4.API.VirtualFS (checkWithImports, emptyVFS, TypeCheckWithDepsResult(..), ResolvedImport(..))
import qualified L4.Evaluate.ValueLazy as ValLazy


#if defined(wasm32_HOST_ARCH)
import GHC.Wasm.Prim (JSString(..), fromJSString, toJSString)
#endif

-- ----------------------------------------------------------------------------
-- Core API Functions
-- ----------------------------------------------------------------------------

-- | Parse and type-check L4 source code with import resolution.
--
-- Returns a JSON array of diagnostics in LSP format:
--
-- @
-- [
--   {
--     "range": { "start": { "line": 0, "character": 0 }, "end": { "line": 0, "character": 10 } },
--     "severity": 1,
--     "message": "Error message",
--     "source": "l4"
--   }
-- ]
-- @
--
-- Severity values: 1=Error, 2=Warning, 3=Info, 4=Hint
--
-- This uses the shared import resolution from 'L4.Import.Resolution':
-- - Embedded core libraries (prelude, math, etc.) are available
-- - User-provided files can be added via VFS (future enhancement)
l4Check :: Text -> Text
l4Check source =
  -- Use checkWithImports for full import resolution support
  case checkWithImports emptyVFS source of
    Left errors ->
      -- Import/parse errors
      encodeJson $ Vector.fromList $ map importErrorToDiagnostic errors
    Right result ->
      -- Type check errors (if any)
      let diagnostics = map checkErrorToDiagnostic result.tcdErrors
      in encodeJson $ Vector.fromList diagnostics

-- | Get hover information at a position.
--
-- Returns JSON-encoded hover info or "null" if nothing to show.
--
-- @
-- {
--   "contents": { "kind": "markdown", "value": "..." },
--   "range": { "start": { "line": 0, "character": 0 }, "end": { "line": 0, "character": 5 } }
-- }
-- @
l4Hover :: Text -> Int -> Int -> Text
l4Hover source line col =
  let pos = MkSrcPos { line = line + 1, column = col + 1 }  -- Convert 0-indexed to 1-indexed
  in case checkWithImports emptyVFS source of
    Left _errors ->
      "null"
    Right result ->
      -- Create a CheckResult structure for the lookup function
      let checkResult = MkCheckResult
            { program = result.tcdModule
            , errors = result.tcdErrors
            , substitution = result.tcdSubstitution
            , environment = result.tcdEnvironment
            , entityInfo = result.tcdEntityInfo
            , mixfixRegistry = result.tcdMixfixRegistry
            , infoMap = result.tcdInfoMap
            , scopeMap = result.tcdScopeMap
            , nlgMap = result.tcdNlgMap
            , descMap = result.tcdDescMap
            }
      -- Use the URI from type checking result to ensure infoMap lookup works
      in case lookupInfoAtPos result.tcdUri pos checkResult of
        Nothing -> "null"
        Just hover -> encodeJson hover

-- | Get completion suggestions at a position.
--
-- Returns a JSON array of completion items:
--
-- @
-- [
--   { "label": "DECIDE", "kind": 14, "detail": "Keyword" },
--   { "label": "GIVEN", "kind": 14, "detail": "Keyword" }
-- ]
-- @
l4Completions :: Text -> Int -> Int -> Text
l4Completions _source _line _col =
  -- For now, return basic L4 keywords
  -- TODO: Implement context-aware completions based on scope
  encodeJson $ Vector.fromList
    [ completionItem "DECIDE" 14 "Define a computed value"
    , completionItem "GIVEN" 14 "Declare function parameters"
    , completionItem "GIVETH" 14 "Declare return type"
    , completionItem "ASSUME" 14 "Declare an assumed value"
    , completionItem "DECLARE" 14 "Declare a data type"
    , completionItem "IF" 14 "Conditional expression"
    , completionItem "THEN" 14 "Then branch"
    , completionItem "ELSE" 14 "Else branch"
    , completionItem "AND" 14 "Boolean conjunction"
    , completionItem "OR" 14 "Boolean disjunction"
    , completionItem "NOT" 14 "Boolean negation"
    , completionItem "IS" 14 "Equality/definition"
    , completionItem "MEANS" 14 "Definition"
    , completionItem "TRUE" 14 "Boolean literal"
    , completionItem "FALSE" 14 "Boolean literal"
    , completionItem "WHERE" 14 "Local definitions"
    , completionItem "LET" 14 "Local binding"
    , completionItem "IN" 14 "Expression body"
    , completionItem "CONSIDER" 14 "Pattern matching"
    , completionItem "WHEN" 14 "Pattern case"
    , completionItem "OTHERWISE" 14 "Default case"
    ]
  where
    completionItem :: Text -> Int -> Text -> Aeson.Value
    completionItem label kind detail = Aeson.object
      [ "label" .= label
      , "kind" .= kind
      , "detail" .= detail
      ]

-- | Get semantic tokens for syntax highlighting.
--
-- Returns delta-encoded token data as per LSP specification.
--
-- @
-- { "data": [0, 0, 6, 0, 0, 1, 0, 5, 1, 0, ...] }
-- @
--
-- Each token is encoded as 5 integers:
-- - deltaLine: line offset from previous token
-- - deltaStartChar: character offset from previous token (or line start)
-- - length: token length
-- - tokenType: index into legend
-- - tokenModifiers: bitmask of modifiers
l4SemanticTokens :: Text -> Text
l4SemanticTokens source =
  let uri = toNormalizedUri (Uri "file:///wasm-input.l4")
  in case execLexer uri source of
    Left _lexErrors ->
      encodeJson $ Aeson.object ["data" .= ([] :: [Int])]
    Right tokens ->
      let encodedTokens = encodeSemanticTokens tokens
      in encodeJson $ Aeson.object ["data" .= encodedTokens]

-- | Evaluate L4 source code and return the results of all #EVAL directives.
--
-- Returns a JSON array of evaluation results:
--
-- @
-- [
--   {
--     "range": { "start": { "line": 0, "character": 0 }, "end": { "line": 0, "character": 10 } },
--     "result": "42",
--     "success": true
--   }
-- ]
-- @
--
-- If there are parse or type errors, returns diagnostics instead.
l4Eval :: Text -> IO Text
l4Eval source =
  case checkWithImports emptyVFS source of
    Left errors ->
      -- Return import/parse errors as diagnostics
      pure $ encodeJson $ Aeson.object
        [ "success" .= False
        , "diagnostics" .= Vector.fromList (map importErrorToDiagnostic errors)
        ]
    Right result ->
      if not (null result.tcdErrors)
        then
          -- Return type check errors as diagnostics
          pure $ encodeJson $ Aeson.object
            [ "success" .= False
            , "diagnostics" .= Vector.fromList (map checkErrorToDiagnostic result.tcdErrors)
            ]
        else do
          -- First, evaluate all imported modules to build up the evaluation environment
          evalConfig <- EL.resolveEvalConfigWithSafeMode Nothing (lspDefaultPolicy defaultGraphVizOptions) True  -- safe mode = True
          
          -- Evaluate each import and collect their environments
          -- We evaluate in order (dependencies first)
          importEnv <- evaluateImports evalConfig result.tcdResolvedImports
          
          -- Now evaluate the main module with the combined import environment
          (_env, results) <- EL.execEvalModuleWithEnv evalConfig result.tcdEntityInfo importEnv result.tcdModule
          pure $ encodeJson $ Aeson.object
            [ "success" .= True
            , "results" .= Vector.fromList (map evalResultToJson results)
            ]

-- | Evaluate a list of resolved imports and combine their environments.
-- Returns the combined evaluation environment.
evaluateImports :: EL.EvalConfig -> [ResolvedImport] -> IO ValLazy.Environment
evaluateImports _evalConfig [] = pure mempty
evaluateImports evalConfig imports = do
  -- Evaluate each import and collect environments
  envs <- forM imports $ \ri -> do
    let tcResult = ri.riTypeChecked
    -- Evaluate this imported module (without any #EVAL directives of its own)
    (env, _) <- EL.execEvalModuleWithEnv evalConfig tcResult.entityInfo mempty tcResult.program
    pure env
  -- Combine all environments (left-biased - later imports override earlier)
  pure $ mconcat envs

-- ----------------------------------------------------------------------------
-- JavaScript FFI Exports (WASM backend only)
-- ----------------------------------------------------------------------------

#if defined(wasm32_HOST_ARCH)

-- | Parse and type-check L4 source code.
foreign export javascript "l4_check"
  js_l4_check :: JSString -> IO JSString

js_l4_check :: JSString -> IO JSString
js_l4_check source = pure $ toJSString $ Text.unpack $ l4Check $ Text.pack $ fromJSString source

-- | Get hover information at a position.
foreign export javascript "l4_hover"
  js_l4_hover :: JSString -> Int -> Int -> IO JSString

js_l4_hover :: JSString -> Int -> Int -> IO JSString
js_l4_hover source line col = pure $ toJSString $ Text.unpack $ l4Hover (Text.pack $ fromJSString source) line col

-- | Get completion suggestions at a position.
foreign export javascript "l4_completions"
  js_l4_completions :: JSString -> Int -> Int -> IO JSString

js_l4_completions :: JSString -> Int -> Int -> IO JSString
js_l4_completions source line col = pure $ toJSString $ Text.unpack $ l4Completions (Text.pack $ fromJSString source) line col

-- | Get semantic tokens for syntax highlighting.
foreign export javascript "l4_semantic_tokens"
  js_l4_semantic_tokens :: JSString -> IO JSString

js_l4_semantic_tokens :: JSString -> IO JSString
js_l4_semantic_tokens source = pure $ toJSString $ Text.unpack $ l4SemanticTokens $ Text.pack $ fromJSString source

-- | Evaluate L4 source code.
foreign export javascript "l4_eval"
  js_l4_eval :: JSString -> IO JSString

js_l4_eval :: JSString -> IO JSString
js_l4_eval source = do
  result <- l4Eval $ Text.pack $ fromJSString source
  pure $ toJSString $ Text.unpack result

-- | Generate ladder diagram visualization for a specific function by name.
foreign export javascript "l4_visualize_by_name"
  js_l4_visualize_by_name :: JSString -> JSString -> Int -> JSString -> Bool -> IO JSString

js_l4_visualize_by_name :: JSString -> JSString -> Int -> JSString -> Bool -> IO JSString
js_l4_visualize_by_name source uri version functionName simplify = pure $ toJSString $ Text.unpack $
  l4VisualizeByName (Text.pack $ fromJSString source) (Text.pack $ fromJSString uri) version (Text.pack $ fromJSString functionName) simplify

-- | Get code lenses for all visualizable DECIDE rules.
foreign export javascript "l4_code_lenses"
  js_l4_code_lenses :: JSString -> JSString -> Int -> IO JSString

js_l4_code_lenses :: JSString -> JSString -> Int -> IO JSString
js_l4_code_lenses source uri version = pure $ toJSString $ Text.unpack $
  l4CodeLenses (Text.pack $ fromJSString source) (Text.pack $ fromJSString uri) version

-- | Go to definition at a position.
foreign export javascript "l4_definition"
  js_l4_definition :: JSString -> Int -> Int -> IO JSString

js_l4_definition :: JSString -> Int -> Int -> IO JSString
js_l4_definition source line col = pure $ toJSString $ Text.unpack $
  l4Definition (Text.pack $ fromJSString source) line col

-- | Find all references at a position.
foreign export javascript "l4_references"
  js_l4_references :: JSString -> Int -> Int -> IO JSString

js_l4_references :: JSString -> Int -> Int -> IO JSString
js_l4_references source line col = pure $ toJSString $ Text.unpack $
  l4References (Text.pack $ fromJSString source) line col

#endif

-- ----------------------------------------------------------------------------
-- Helper Functions
-- ----------------------------------------------------------------------------

-- | Encode a value to JSON text.
encodeJson :: Aeson.ToJSON a => a -> Text
encodeJson = LazyText.toStrict . LazyText.decodeUtf8 . Aeson.encode

-- | Convert an import/resolution error to an LSP diagnostic.
importErrorToDiagnostic :: Text -> Aeson.Value
importErrorToDiagnostic errMsg =
  Aeson.object
    [ "range" .= defaultRangeJson
    , "severity" .= (1 :: Int)  -- Error
    , "message" .= errMsg
    , "source" .= ("l4" :: Text)
    ]
  where
    defaultRangeJson = Aeson.object
      [ "start" .= Aeson.object ["line" .= (0 :: Int), "character" .= (0 :: Int)]
      , "end" .= Aeson.object ["line" .= (0 :: Int), "character" .= (0 :: Int)]
      ]

-- | Convert a type check error to an LSP diagnostic.
checkErrorToDiagnostic :: CheckErrorWithContext -> Aeson.Value
checkErrorToDiagnostic err =
  Aeson.object
    [ "range" .= rangeToJson (errorRange err)
    , "severity" .= severityToInt (severity err)
    , "message" .= Text.intercalate "\n" (prettyCheckErrorWithContext err)
    , "source" .= ("l4" :: Text)
    ]

-- | Get the source range for an error, with a default fallback.
errorRange :: CheckErrorWithContext -> SrcRange
errorRange err = fromMaybe defaultRange (rangeOf err)
  where
    defaultRange = MkSrcRange
      { start = MkSrcPos 1 1
      , end = MkSrcPos 1 1
      , moduleUri = toNormalizedUri (Uri "file:///wasm-input.l4")
      , length = 0
      }

-- | Convert a SrcRange to JSON (LSP Range format, 0-indexed).
rangeToJson :: SrcRange -> Aeson.Value
rangeToJson range = Aeson.object
  [ "start" .= posToJson range.start
  , "end" .= posToJson range.end
  ]

-- | Convert a SrcPos to JSON (LSP Position format, 0-indexed).
posToJson :: SrcPos -> Aeson.Value
posToJson pos = Aeson.object
  [ "line" .= (pos.line - 1)        -- Convert 1-indexed to 0-indexed
  , "character" .= (pos.column - 1)  -- Convert 1-indexed to 0-indexed
  ]

-- | Convert severity to LSP DiagnosticSeverity integer.
severityToInt :: Severity -> Int
severityToInt SError = 1
severityToInt SWarn  = 2
severityToInt SInfo  = 3

-- | Look up type/hover information at a source position.
-- Queries the infoMap from CheckResult using interval lookup.
lookupInfoAtPos :: NormalizedUri -> SrcPos -> CheckResult -> Maybe Aeson.Value
lookupInfoAtPos nuri pos result =
  case IV.smallestContaining nuri pos result.infoMap of
    Nothing -> Nothing
    Just (srcRange, info) ->
      Just $ infoToHover nuri result.substitution srcRange info

-- | Convert type info to LSP hover format.
infoToHover :: NormalizedUri -> Substitution -> SrcRange -> Info -> Aeson.Value
infoToHover nuri subst srcRange info = Aeson.object
  [ "contents" .= Aeson.object
      [ "kind" .= ("markdown" :: Text)
      , "value" .= mdCodeBlock (infoToText nuri subst info)
      ]
  , "range" .= rangeToJson srcRange
  ]

-- | Convert Info to display text.
infoToText :: NormalizedUri -> Substitution -> Info -> Text
infoToText nuri subst = \case
  TypeInfo ty _mTermKind ->
    prettyLayout (applyFinalSubstitution subst nuri ty)
  KindInfo kind ->
    prettyLayout (typeFunction kind)
  TypeVariable ->
    "TYPE VARIABLE"

-- | Format as markdown code block.
mdCodeBlock :: Text -> Text
mdCodeBlock c = Text.unlines ["```", c, "```"]

-- | A kind of a type is its arity - convert to a Type for display.
-- (e.g., kind 0 = TYPE, kind 1 = FUNCTION FROM TYPE TO TYPE)
typeFunction :: Int -> L4.Syntax.Type' L4.Syntax.Resolved
typeFunction 0 = L4.Syntax.Type L4.Annotation.emptyAnno
typeFunction n | n > 0 =
  L4.Syntax.Fun L4.Annotation.emptyAnno
    (replicate n (L4.Syntax.MkOptionallyNamedType L4.Annotation.emptyAnno Nothing (L4.Syntax.Type L4.Annotation.emptyAnno)))
    (L4.Syntax.Type L4.Annotation.emptyAnno)
typeFunction _ = error "Internal error: negative arity of type constructor"

-- | Convert an evaluation result to JSON.
evalResultToJson :: EL.EvalDirectiveResult -> Aeson.Value
evalResultToJson edr = Aeson.object $
  [ "result" .= prettyEvalResult edr.result
  , "success" .= isSuccess edr.result
  ] ++
  case edr.range of
    Nothing -> []
    Just r -> ["range" .= rangeToJson r]
  where
    isSuccess (EL.Assertion b) = b
    isSuccess (EL.Reduction (Right _)) = True
    isSuccess (EL.Reduction (Left _)) = False

-- | Pretty print an evaluation directive result value.
prettyEvalResult :: EL.EvalDirectiveValue -> Text
prettyEvalResult (EL.Assertion True)       = "assertion satisfied"
prettyEvalResult (EL.Assertion False)      = "assertion failed"
prettyEvalResult (EL.Reduction (Left exc)) = Text.unlines (prettyEvalException exc)
prettyEvalResult (EL.Reduction (Right v))  = prettyLayout v

-- | Generate ladder diagram visualization data for a specific DECIDE rule by name.
--
-- Same as 'l4Visualize' but targets a specific function by name.
-- Used for auto-refresh to update the currently selected function.
--
-- @
-- // Success:
-- {
--   "verDocId": { "uri": "...", "version": 0 },
--   "funDecl": { ... }
-- }
--
-- // Error (function not found or not visualizable):
-- {
--   "error": "...",
--   "notFound": true  // indicates function no longer exists
-- }
-- @
l4VisualizeByName :: Text -> Text -> Int -> Text -> Bool -> Text
l4VisualizeByName source uriText version functionName simplify =
  case checkWithImports emptyVFS source of
    Left errors ->
      encodeJson $ Aeson.object
        [ "error" .= Text.intercalate "; " errors
        ]
    Right result ->
      if not (null result.tcdErrors)
        then
          encodeJson $ Aeson.object
            [ "error" .= ("Type check error" :: Text)
            ]
        else
          -- Use the URI from type checking result to ensure substitution lookup works correctly
          case Ladder.visualizeByName result.tcdUri uriText version result.tcdModule result.tcdSubstitution simplify functionName of
            Left Ladder.InvalidProgramNoDecidesFound ->
              encodeJson $ Aeson.object
                [ "error" .= ("Function '" <> functionName <> "' not found or not visualizable" :: Text)
                , "notFound" .= True
                ]
            Left vizError ->
              encodeJson $ Aeson.object
                [ "error" .= Ladder.prettyPrintVizError vizError
                ]
            Right ladderInfo ->
              encodeJson ladderInfo

-- | Get code lenses for all visualizable DECIDE rules.
--
-- Returns JSON array of code lens objects for Monaco:
--
-- @
-- [
--   {
--     "range": { "startLineNumber": 1, "startColumn": 1, "endLineNumber": 1, "endColumn": 1 },
--     "command": {
--       "id": "l4.visualize",
--       "title": "Visualize",
--       "arguments": [{ "uri": "...", "version": 0 }, "functionName", false]
--     }
--   }
-- ]
-- @
l4CodeLenses :: Text -> Text -> Int -> Text
l4CodeLenses source uriText version =
  case checkWithImports emptyVFS source of
    Left _errors ->
      -- Parse/import errors - return empty array (diagnostics shown separately)
      encodeJson ([] :: [Aeson.Value])
    Right result ->
      if not (null result.tcdErrors)
        then
          -- Type check errors - return empty array (diagnostics shown separately)
          encodeJson ([] :: [Aeson.Value])
        else
          -- Use the URI from type checking result to ensure substitution lookup works correctly
          let decides = Ladder.findAllVisualizableDecides result.tcdUri result.tcdModule result.tcdSubstitution
              lenses = concatMap (mkCodeLenses uriText version) decides
          in encodeJson lenses
  where
    mkCodeLenses :: Text -> Int -> Ladder.VisualizableDecide -> [Aeson.Value]
    mkCodeLenses uriTxt ver vd =
      -- Generate two code lenses: "Visualize" and "Simplify and visualize"
      [ mkCodeLens uriTxt ver vd False
      , mkCodeLens uriTxt ver vd True
      ]

    mkCodeLens :: Text -> Int -> Ladder.VisualizableDecide -> Bool -> Aeson.Value
    mkCodeLens uriTxt ver vd simplify = Aeson.object
      [ "range" .= Aeson.object
          -- Monaco uses 1-indexed positions
          [ "startLineNumber" .= vd.vdStartLine
          , "startColumn" .= vd.vdStartCol
          , "endLineNumber" .= vd.vdStartLine
          , "endColumn" .= vd.vdStartCol
          ]
      , "command" .= Aeson.object
          [ "id" .= ("l4.visualize" :: Text)
          , "title" .= if simplify then ("Simplify and visualize" :: Text) else ("Visualize" :: Text)
          , "arguments" .= 
              [ Aeson.object [ "uri" .= uriTxt, "version" .= ver ]
              , Aeson.String vd.vdName
              , Aeson.Bool simplify
              ]
          ]
      ]

-- | Go to definition at a position.
--
-- Returns JSON-encoded location or null:
--
-- @
-- // Found:
-- {
--   "range": {
--     "start": { "line": 0, "character": 0 },
--     "end": { "line": 0, "character": 10 }
--   }
-- }
--
-- // Not found:
-- null
-- @
l4Definition :: Text -> Int -> Int -> Text
l4Definition source line col =
  let pos = MkSrcPos line col
  in case checkWithImports emptyVFS source of
    Left _errors -> "null"
    Right result ->
      case findDefinition pos result.tcdModule of
        Nothing -> "null"
        Just range -> encodeJson $ srcRangeToLspRange range

-- | Find all references at a position.
--
-- Returns JSON-encoded array of locations:
--
-- @
-- [
--   {
--     "range": {
--       "start": { "line": 0, "character": 0 },
--       "end": { "line": 0, "character": 10 }
--     }
--   }
-- ]
-- @
l4References :: Text -> Int -> Int -> Text
l4References source line col =
  let pos = MkSrcPos line col
  in case checkWithImports emptyVFS source of
    Left _errors -> encodeJson ([] :: [Aeson.Value])
    Right result ->
      let ranges = findReferences pos result.tcdModule
          locs = map srcRangeToLspRange ranges
      in encodeJson locs

-- | Convert a SrcRange to LSP range format (0-indexed).
srcRangeToLspRange :: SrcRange -> Aeson.Value
srcRangeToLspRange (MkSrcRange (MkSrcPos startLine startCol) (MkSrcPos endLine endCol) _ _) =
  Aeson.object
    [ "range" .= Aeson.object
        [ "start" .= Aeson.object
            [ "line" .= (startLine - 1)  -- Convert to 0-indexed
            , "character" .= (startCol - 1)
            ]
        , "end" .= Aeson.object
            [ "line" .= (endLine - 1)
            , "character" .= (endCol - 1)
            ]
        ]
    ]

-- | Encode lexer tokens as LSP semantic tokens.
--
-- Token types (indices into legend):
-- 0=keyword, 1=type, 2=function, 3=variable, 4=number,
-- 5=string, 6=operator, 7=comment, 8=decorator
encodeSemanticTokens :: [PosToken] -> [Int]
encodeSemanticTokens tokens = go 0 0 tokens
  where
    go :: Int -> Int -> [PosToken] -> [Int]
    go _prevLine _prevCol [] = []
    go prevLine prevCol (tok : rest) =
      case tokenToSemanticType tok of
        Nothing -> go prevLine prevCol rest  -- Skip non-semantic tokens
        Just tokenType ->
          let currLine = tok.range.start.line - 1  -- Convert to 0-indexed
              currCol = tok.range.start.column - 1
              deltaLine = currLine - prevLine
              deltaCol = if deltaLine == 0 then currCol - prevCol else currCol
              len = tok.range.length
          in deltaLine : deltaCol : len : tokenType : 0  -- 0 = no modifiers
             : go currLine currCol rest

-- | Map a token to its semantic token type index.
-- Returns Nothing for tokens that shouldn't be highlighted.
tokenToSemanticType :: PosToken -> Maybe Int
tokenToSemanticType tok = case tok.payload of
  -- Keywords (type index 0)
  TKeywords _ -> Just 0
  
  -- Identifiers (type index 3 for variables)
  -- Note: We don't distinguish types from variables at the lexer level
  -- Type-based highlighting would require type checker results
  TIdentifiers (TIdentifier _) -> Just 3
  TIdentifiers (TQuoted _) -> Just 3
  TIdentifiers TGenitive -> Just 3
  
  -- Literals
  TLiterals (TIntLit _ _) -> Just 4      -- Numbers (type index 4)
  TLiterals (TRationalLit _ _) -> Just 4 -- Numbers (type index 4)
  TLiterals (TStringLit _) -> Just 5     -- Strings (type index 5)
  
  -- Operators (type index 6)
  TOperators _ -> Just 6
  
  -- Comments (type index 7)
  TSpaces (TLineComment _) -> Just 7
  TSpaces (TBlockComment _) -> Just 7
  
  -- Annotations/decorators (type index 8)
  TAnnotations _ -> Just 8
  
  -- Skip whitespace, symbols, directives, etc.
  TSpaces _ -> Nothing
  TSymbols _ -> Nothing
  TDirectives _ -> Nothing
