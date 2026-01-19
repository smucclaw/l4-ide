{-# LANGUAGE CPP #-}
-- | WASM/JavaScript FFI exports for browser-based L4 language features.
--
-- This module provides the interface between the L4 language implementation
-- and the browser via GHC's JavaScript FFI. The exports are only compiled
-- when targeting the JavaScript/WASM backend.
--
-- The TypeScript bridge in @ts-apps/jl4-web/src/lib/wasm/wasm-bridge.ts@
-- calls these functions and expects JSON-encoded responses.
--
-- @since 0.1
module L4.Wasm
  (
#ifdef ghcjs_HOST_OS
    -- * FFI Exports (WASM/JS only)
    -- These are exported via @foreign export javascript@ and callable from JS.
    -- They are not visible to Haskell code on native platforms.
#else
    -- * API Functions
    -- On native platforms, these functions are available for testing.
    l4Check
  , l4Hover
  , l4Completions
  , l4SemanticTokens
  , l4Eval
#endif
  ) where

import Base
import qualified Base.Text as Text
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LazyText
import qualified Data.Vector as Vector

import L4.Lexer (execLexer, PosToken(..), TokenType(..), TLiterals(..), TSpaces(..), TIdentifiers(..), PError(..))
import L4.Parser (execProgramParserWithHintPass)
import L4.Annotation (HasSrcRange(..))
import L4.Parser.SrcSpan (SrcRange(..), SrcSpan(..), SrcPos(..))
import L4.TypeCheck (doCheckProgram, severity, prettyCheckErrorWithContext, applyFinalSubstitution)
import L4.TypeCheck.Types (CheckResult(..), CheckErrorWithContext(..), Severity(..), Substitution)
import L4.Print (prettyLayout)
import L4.Syntax (Info(..), Type'(..), Resolved, OptionallyNamedType(..))
import L4.Annotation (emptyAnno)
import qualified L4.Utils.IntervalMap as IV
import qualified L4.EvaluateLazy as EL
import L4.EvaluateLazy.Machine (prettyEvalException)

import L4.TracePolicy (lspDefaultPolicy)
import L4.EvaluateLazy.GraphVizOptions (defaultGraphVizOptions)


#ifdef ghcjs_HOST_OS
import GHC.JS.Prim (JSVal, fromJSString, toJSString)
#endif

-- ----------------------------------------------------------------------------
-- Core API Functions
-- ----------------------------------------------------------------------------

-- | Parse and type-check L4 source code.
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
l4Check :: Text -> Text
l4Check source =
  let uri = toNormalizedUri (Uri "file:///wasm-input.l4")
  in case execProgramParserWithHintPass uri source of
    Left parseErrors ->
      -- Convert parse errors to diagnostics
      encodeJson $ Vector.fromList $ map parseErrorToDiagnostic (toList parseErrors)
    Right (parsed, _hints, _parseWarnings) ->
      -- Type check the parsed module
      let checkResult = doCheckProgram uri parsed
          diagnostics = map checkErrorToDiagnostic checkResult.errors
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
  let uri = toNormalizedUri (Uri "file:///wasm-input.l4")
      pos = MkSrcPos { line = line + 1, column = col + 1 }  -- Convert 0-indexed to 1-indexed
  in case execProgramParserWithHintPass uri source of
    Left _parseErrors ->
      "null"
    Right (parsed, _hints, _) ->
      let checkResult = doCheckProgram uri parsed
      in case lookupInfoAtPos uri pos checkResult of
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
l4Eval source = do
  let uri = toNormalizedUri (Uri "file:///wasm-input.l4")
  case execProgramParserWithHintPass uri source of
    Left parseErrors ->
      -- Return parse errors as diagnostics
      pure $ encodeJson $ Aeson.object
        [ "success" .= False
        , "diagnostics" .= Vector.fromList (map parseErrorToDiagnostic (toList parseErrors))
        ]
    Right (parsed, _hints, _parseWarnings) ->
      let checkResult = doCheckProgram uri parsed
      in if not (null checkResult.errors)
        then
          -- Return type check errors as diagnostics
          pure $ encodeJson $ Aeson.object
            [ "success" .= False
            , "diagnostics" .= Vector.fromList (map checkErrorToDiagnostic checkResult.errors)
            ]
        else do
          -- Evaluate the module
          evalConfig <- EL.resolveEvalConfigWithSafeMode Nothing (lspDefaultPolicy defaultGraphVizOptions) True  -- safe mode = True
          (_env, results) <- EL.execEvalModuleWithEnv evalConfig checkResult.entityInfo mempty checkResult.program
          pure $ encodeJson $ Aeson.object
            [ "success" .= True
            , "results" .= Vector.fromList (map evalResultToJson results)
            ]

-- ----------------------------------------------------------------------------
-- JavaScript FFI Exports (WASM/JS backend only)
-- ----------------------------------------------------------------------------

#ifdef ghcjs_HOST_OS

-- | Parse and type-check L4 source code.
foreign export javascript "l4_check"
  js_l4_check :: JSVal -> IO JSVal

js_l4_check :: JSVal -> IO JSVal
js_l4_check sourceVal = do
  let source = Text.pack $ fromJSString sourceVal
  pure $ toJSString $ Text.unpack $ l4Check source

-- | Get hover information at a position.
foreign export javascript "l4_hover"
  js_l4_hover :: JSVal -> Int -> Int -> IO JSVal

js_l4_hover :: JSVal -> Int -> Int -> IO JSVal
js_l4_hover sourceVal line col = do
  let source = Text.pack $ fromJSString sourceVal
  pure $ toJSString $ Text.unpack $ l4Hover source line col

-- | Get completion suggestions at a position.
foreign export javascript "l4_completions"
  js_l4_completions :: JSVal -> Int -> Int -> IO JSVal

js_l4_completions :: JSVal -> Int -> Int -> IO JSVal
js_l4_completions sourceVal line col = do
  let source = Text.pack $ fromJSString sourceVal
  pure $ toJSString $ Text.unpack $ l4Completions source line col

-- | Get semantic tokens for syntax highlighting.
foreign export javascript "l4_semantic_tokens"
  js_l4_semantic_tokens :: JSVal -> IO JSVal

js_l4_semantic_tokens :: JSVal -> IO JSVal
js_l4_semantic_tokens sourceVal = do
  let source = Text.pack $ fromJSString sourceVal
  pure $ toJSString $ Text.unpack $ l4SemanticTokens source

-- | Evaluate L4 source code.
foreign export javascript "l4_eval"
  js_l4_eval :: JSVal -> IO JSVal

js_l4_eval :: JSVal -> IO JSVal
js_l4_eval sourceVal = do
  let source = Text.pack $ fromJSString sourceVal
  result <- l4Eval source
  pure $ toJSString $ Text.unpack result

#endif

-- ----------------------------------------------------------------------------
-- Helper Functions
-- ----------------------------------------------------------------------------

-- | Encode a value to JSON text.
encodeJson :: Aeson.ToJSON a => a -> Text
encodeJson = LazyText.toStrict . LazyText.decodeUtf8 . Aeson.encode

-- | Convert a parse error to an LSP diagnostic.
parseErrorToDiagnostic :: PError -> Aeson.Value
parseErrorToDiagnostic err =
  Aeson.object
    [ "range" .= spanToJson err.range
    , "severity" .= (1 :: Int)  -- Error
    , "message" .= err.message
    , "source" .= ("l4" :: Text)
    ]

-- | Convert a SrcSpan to JSON (LSP Range format, 0-indexed).
spanToJson :: SrcSpan -> Aeson.Value
spanToJson srcSpan = Aeson.object
  [ "start" .= posToJson srcSpan.start
  , "end" .= posToJson srcSpan.end
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
infoToHover :: NormalizedUri -> L4.TypeCheck.Types.Substitution -> SrcRange -> Info -> Aeson.Value
infoToHover nuri subst srcRange info = Aeson.object
  [ "contents" .= Aeson.object
      [ "kind" .= ("markdown" :: Text)
      , "value" .= mdCodeBlock (infoToText nuri subst info)
      ]
  , "range" .= rangeToJson srcRange
  ]

-- | Convert Info to display text.
infoToText :: NormalizedUri -> L4.TypeCheck.Types.Substitution -> Info -> Text
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
