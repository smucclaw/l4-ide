-- | Basic tests for the L4 MLIR compiler.
--
-- Tests the MLIR IR construction and emission (no external toolchain needed).
-- Integration tests that require mlir-opt / wasmtime are separate.
module Main (main) where

import qualified Data.Text as Text
import System.Exit (exitFailure, exitSuccess)

import qualified Data.Text as T

import L4.MLIR.IR
import L4.MLIR.Emit (renderMLIR)
import L4.MLIR.Dialect.Func
import L4.MLIR.Dialect.Arith
import L4.MLIR.Dialect.SCF
import L4.MLIR.Runtime.Builtins (builtinDeclarations)
import L4.MLIR.Lower (lowerProgramWithInfo, lowerProgramWithDiagnostics)
import L4.MLIR.Schema (bundleExports, applyDiagnostics, encodeBundle)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as TE

import L4.API.VirtualFS (checkWithImports, checkWithImportsAndUri, emptyVFS, vfsFromList)
import L4.Import.Resolution (TypeCheckWithDepsResult(..), ResolvedImport(..))
import L4.TypeCheck (applyFinalSubstitution)
import L4.TypeCheck.Types (CheckResult(..), EntityInfo)

-- | 'L4.Import.Resolution.typecheckWithDependencies' leaves the main
-- module's @entityInfo@ un-substituted (it applies the substitution
-- only to imported modules), so test consumers like
-- 'enrichReturnTypes' would see 'InfVar' rather than the resolved
-- 'TyApp' for a @MEANS@-binding's inferred return type. The
-- production pipeline (LSP rules) substitutes via
-- 'applyFinalSubstitution' — mirror that here.
substitutedEntityInfo :: TypeCheckWithDepsResult -> EntityInfo
substitutedEntityInfo r =
  applyFinalSubstitution r.tcdSubstitution r.tcdUri r.tcdEntityInfo

main :: IO ()
main = do
  results <- sequence
    [ test "type emission"                     testTypeEmission
    , test "simple function"                   testSimpleFunction
    , test "if-then-else"                      testIfThenElse
    , test "builtin declarations"              testBuiltins
    , test "arithmetic ops"                    testArithOps
    , test "@export ASSUMEs become args"       testExportAssumePromotion
    , test "internal callers fill via extern"  testInternalCallFillsAssumes
    , test "schema picks up imported DECLAREs" testSchemaUsesImportedDeclares
    , test "deontic fn flagged unsupported"    testDeonticFlaggedUnsupported
    , test "plain fn stays supported"          testPlainFunctionSupported
    , test "state graph baked into schema"     testStateGraphBaked
    , test "no state graph for plain fn"       testNoStateGraphForPlainFn
    , test "traceMeta pretty-print baked"      testTraceMetaBaked
    , test "<fn>$trace symbol emitted"         testTraceSymbolEmitted
    , test "traceMeta nodes populated"         testTraceMetaNodes
    , test "AND/OR/NOT marked special"         testTraceSpecialMarkers
    , test "fnValue node + enter_fn/exit_fn"   testTraceFnValueAndContext
    , test "NOT-range disambiguation"          testTraceNotRangeDisambiguation
    , test "returnType enriched from EntityInfo" testReturnTypeEnrichedFromEntityInfo
    ]
  if and results
    then do
      putStrLn $ "\nAll " <> show (length results) <> " tests passed."
      exitSuccess
    else do
      putStrLn $ "\nFailed: " <> show (length (filter not results)) <> " of " <> show (length results)
      exitFailure

test :: String -> IO Bool -> IO Bool
test name action = do
  putStr $ "  " <> name <> "... "
  result <- action
  putStrLn $ if result then "ok" else "FAIL"
  pure result

-- | Test that MLIR types render correctly.
testTypeEmission :: IO Bool
testTypeEmission = do
  let mlir = renderMLIR $ MLIRModule
        { moduleOps = [funcFunc "test_types"
            [(0, FloatType 64), (1, IntegerType 1), (2, PointerType)]
            [FloatType 64]
            (Region [Block 0
              [(0, FloatType 64), (1, IntegerType 1), (2, PointerType)]
              [funcReturn [SSAValue 0] [FloatType 64]]])]
        }
  -- Check that the output contains expected type strings
  pure $ Text.isInfixOf "f64" mlir
      && Text.isInfixOf "i1" mlir
      && Text.isInfixOf "!llvm.ptr" mlir

-- | Test a simple function definition.
testSimpleFunction :: IO Bool
testSimpleFunction = do
  let mlir = renderMLIR $ MLIRModule
        { moduleOps =
            [ funcFunc "add_numbers"
                [(0, FloatType 64), (1, FloatType 64)]
                [FloatType 64]
                (Region [Block 0
                  [(0, FloatType 64), (1, FloatType 64)]
                  [ arithAddf 2 (SSAValue 0) (SSAValue 1)
                  , funcReturn [SSAValue 2] [FloatType 64]
                  ]])
            ]
        }
  pure $ Text.isInfixOf "func.func @add_numbers" mlir
      && Text.isInfixOf "arith.addf" mlir
      && Text.isInfixOf "func.return" mlir

-- | Test if-then-else lowering via scf.if.
testIfThenElse :: IO Bool
testIfThenElse = do
  let thenRegion = Region [Block 0 []
        [ arithConstantFloat 10 42.0
        , scfYield [SSAValue 10] [FloatType 64]
        ]]
      elseRegion = Region [Block 0 []
        [ arithConstantFloat 11 0.0
        , scfYield [SSAValue 11] [FloatType 64]
        ]]
      mlir = renderMLIR $ MLIRModule
        { moduleOps =
            [ funcFunc "conditional"
                [(0, IntegerType 1)]
                [FloatType 64]
                (Region [Block 0
                  [(0, IntegerType 1)]
                  [ scfIf [1] (SSAValue 0) thenRegion elseRegion [FloatType 64]
                  , funcReturn [SSAValue 1] [FloatType 64]
                  ]])
            ]
        }
  pure $ Text.isInfixOf "scf.if" mlir
      && Text.isInfixOf "scf.yield" mlir

-- | Test that builtin runtime declarations generate valid MLIR.
testBuiltins :: IO Bool
testBuiltins = do
  let mlir = renderMLIR $ MLIRModule { moduleOps = builtinDeclarations }
  pure $ Text.isInfixOf "__l4_pow" mlir
      && Text.isInfixOf "__l4_str_concat" mlir
      && Text.isInfixOf "__l4_alloc" mlir

-- | Test arithmetic operation generation.
testArithOps :: IO Bool
testArithOps = do
  let mlir = renderMLIR $ MLIRModule
        { moduleOps =
            [ funcFunc "math_ops"
                [(0, FloatType 64), (1, FloatType 64)]
                [FloatType 64]
                (Region [Block 0
                  [(0, FloatType 64), (1, FloatType 64)]
                  [ arithAddf 2 (SSAValue 0) (SSAValue 1)
                  , arithSubf 3 (SSAValue 2) (SSAValue 1)
                  , arithMulf 4 (SSAValue 3) (SSAValue 0)
                  , arithDivf 5 (SSAValue 4) (SSAValue 1)
                  , arithCmpf 6 OGT (SSAValue 5) (SSAValue 0)
                  , funcReturn [SSAValue 5] [FloatType 64]
                  ]])
            ]
        }
  pure $ Text.isInfixOf "arith.addf" mlir
      && Text.isInfixOf "arith.subf" mlir
      && Text.isInfixOf "arith.mulf" mlir
      && Text.isInfixOf "arith.divf" mlir

-- | Lower an L4 source string to MLIR text, going through the real
-- typecheck + lowering pipeline.
lowerSource :: T.Text -> Either [T.Text] T.Text
lowerSource src =
  case checkWithImports emptyVFS src of
    Left errs -> Left errs
    Right r -> Right (renderMLIR (lowerProgramWithInfo r.tcdInfoMap r.tcdModule []))

-- | Lower a source and render its schema JSON *with* per-function
-- lowering diagnostics applied (the @supported@ / @unsupportedReason@
-- flags). Mirrors what 'L4.MLIR.Pipeline' does.
schemaWithDiagnostics :: T.Text -> Either [T.Text] T.Text
schemaWithDiagnostics src =
  case checkWithImports emptyVFS src of
    Left errs -> Left errs
    Right r ->
      let (_mlir, diags) = lowerProgramWithDiagnostics r.tcdInfoMap r.tcdModule []
          bundle = applyDiagnostics diags (bundleExports "test.wasm" "test" r.tcdInfoMap (substitutedEntityInfo r) r.tcdModule [])
      in Right (TE.decodeUtf8 (LBS.toStrict (encodeBundle bundle)))

-- | A DEONTIC/regulative function can't be faithfully compiled, so the
-- schema must mark it @supported: false@ with a reason — rather than the
-- old behaviour of silently lowering the body to FALSE. (M1a)
testDeonticFlaggedUnsupported :: IO Bool
testDeonticFlaggedUnsupported = do
  let src = T.unlines
        [ "DECLARE Party IS ONE OF `alice`"
        , "DECLARE Action IS ONE OF `pay`"
        , ""
        , "@export Demo obligation"
        , "GIVETH A DEONTIC OF Party, Action"
        , "`demo` MEANS"
        , "    PARTY `alice`"
        , "    MUST `pay`"
        , "    WITHIN 10"
        , "    HENCE FULFILLED"
        ]
  case schemaWithDiagnostics src of
    Left errs -> do
      putStrLn $ "\n    typecheck failed: " <> show errs
      pure False
    Right json ->
      pure $ T.isInfixOf "\"supported\":false" json
          && T.isInfixOf "not supported by the WASM backend" json

-- | A plain decidable function lowers cleanly, so it must stay
-- @supported: true@ and emit no unsupported flag. (M1a negative control)
testPlainFunctionSupported :: IO Bool
testPlainFunctionSupported = do
  let src = T.unlines
        [ "@export Adult check"
        , "GIVEN `age` IS A NUMBER"
        , "GIVETH A BOOLEAN"
        , "DECIDE `is_adult` IF `age` >= 18"
        ]
  case schemaWithDiagnostics src of
    Left errs -> do
      putStrLn $ "\n    typecheck failed: " <> show errs
      pure False
    Right json ->
      pure $ T.isInfixOf "\"supported\":true" json
          && not (T.isInfixOf "\"supported\":false" json)

-- | A top-level regulative function yields a precomputed state graph in
-- the schema, with Graphviz DOT rendered at compile time by the same
-- pure function jl4-service uses. (M3)
testStateGraphBaked :: IO Bool
testStateGraphBaked = do
  let src = T.unlines
        [ "DECLARE Party IS ONE OF `alice`"
        , "DECLARE Action IS ONE OF `pay`"
        , ""
        , "@export Demo obligation"
        , "GIVETH A DEONTIC OF Party, Action"
        , "`demo` MEANS"
        , "    PARTY `alice`"
        , "    MUST `pay`"
        , "    WITHIN 10"
        , "    HENCE FULFILLED"
        ]
  case checkWithImports emptyVFS src of
    Left errs -> do
      putStrLn $ "\n    typecheck failed: " <> show errs
      pure False
    Right r -> do
      let bundle = bundleExports "test.wasm" "test" r.tcdInfoMap (substitutedEntityInfo r) r.tcdModule []
          json = TE.decodeUtf8 (LBS.toStrict (encodeBundle bundle))
      pure $ T.isInfixOf "\"stateGraphs\"" json
          && T.isInfixOf "\"name\":\"demo\"" json
          && T.isInfixOf "digraph" json

-- | M5 slice 2A: every exported function's schema entry carries a
-- @traceMeta@ block with the function name, body, and parameter
-- references pre-rendered via 'L4.Print.prettyLayout' — the exact
-- strings jl4-service's interpreter uses inside @Reasoning.exampleCode@.
-- The runtime consumes these to synthesise a trace tree at request time
-- without re-implementing the pretty-printer in JavaScript.
testTraceMetaBaked :: IO Bool
testTraceMetaBaked = do
  let src = T.unlines
        [ "@export Is eligible check"
        , "GIVEN `years of service` IS A NUMBER"
        , "GIVETH A BOOLEAN"
        , "DECIDE `is eligible` IF `years of service` >= 3"
        ]
  case checkWithImports emptyVFS src of
    Left errs -> do
      putStrLn $ "\n    typecheck failed: " <> show errs
      pure False
    Right r -> do
      let bundle = bundleExports "test.wasm" "test" r.tcdInfoMap (substitutedEntityInfo r) r.tcdModule []
          json = TE.decodeUtf8 (LBS.toStrict (encodeBundle bundle))
      -- The backticked function name (matches jl4-service's prettyLayout
      -- of the AppForm head): a multi-word name is wrapped in backticks.
      let okFnName = T.isInfixOf "\"fnName\":\"`is eligible`\"" json
      -- The body comes out as `<lhs> AT LEAST <rhs>` after prelude
      -- operator rendering. Spot-check that the source-level operator
      -- name and the backticked parameter reference appear; full byte
      -- equality is what the parity harness's trace sub-matrix tracks
      -- downstream as later slices land.
      let okBody = T.isInfixOf "AT LEAST" json
                && T.isInfixOf "`years of service`" json
      let okParams = T.isInfixOf "\"params\":[\"years of service\"]" json
      pure (okFnName && okBody && okParams)

-- | M5 slice 2C: every export carries a @traceMeta.nodes@ array with
-- one 'TraceNode' per traceable subexpression — pre-rendered via
-- 'L4.Print.prettyLayout' and tagged with a 'resultKind' the runtime
-- uses to render @Result: …@ lines. The walk skips 'Lit' and zero-arg
-- @App@ (the 'Var' pattern), mirroring jl4-service's
-- @simplifyEvalTrace@ pruning.
testTraceMetaNodes :: IO Bool
testTraceMetaNodes = do
  -- An IF body with a numeric arithmetic branch — that gives us three
  -- traceable subexpressions: the IfThenElse itself, the Geq guard, and
  -- the Plus body. Skipped: the Lit 0 / Lit 1 / Lit 2 (Lit) and the
  -- Var 'age' references (zero-arg App).
  let src = T.unlines
        [ "@export Bonus point"
        , "GIVEN `age` IS A NUMBER"
        , "GIVETH A NUMBER"
        , "DECIDE `bonus` IS IF `age` >= 18 THEN `age` PLUS 1 ELSE 0"
        ]
  case checkWithImports emptyVFS src of
    Left errs -> do
      putStrLn $ "\n    typecheck failed: " <> show errs
      pure False
    Right r -> do
      let bundle = bundleExports "test.wasm" "test" r.tcdInfoMap (substitutedEntityInfo r) r.tcdModule []
          json = TE.decodeUtf8 (LBS.toStrict (encodeBundle bundle))
      -- IfThenElse → kind 0 (NUMBER, via InfoMap on the whole expression).
      -- Geq → kind 1 (BOOLEAN). Plus → kind 0.
      let okIf  = T.isInfixOf "\"id\":0" json && T.isInfixOf "IF " json
          okGeq = T.isInfixOf "AT LEAST" json
                && T.isInfixOf "\"resultKind\":1" json
          okAdd = T.isInfixOf "PLUS" json
                && T.isInfixOf "\"resultKind\":0" json
      pure (okIf && okGeq && okAdd)

-- | M5 slice 2B: every lowered Decide gets a second @func.func@ named
-- @<fn>$trace@ that brackets the body with per-subexpression
-- @__l4_trace_enter(id)@ / @__l4_trace_exit(boxed, kind)@. The runtime
-- calls into this when the request asks for @?trace=full@; the untraced
-- @<fn>@ stays the fast path.
testTraceSymbolEmitted :: IO Bool
testTraceSymbolEmitted = do
  let src = T.unlines
        [ "@export Adult check"
        , "GIVEN `age` IS A NUMBER"
        , "GIVETH A BOOLEAN"
        , "DECIDE `is_adult` IF `age` >= 18"
        ]
  case lowerSource src of
    Left errs -> do
      putStrLn $ "\n    typecheck failed: " <> show errs
      pure False
    Right mlir ->
      -- Both symbols must exist, and the trace clone must contain the
      -- runtime calls. The trace exit's kind byte for BOOLEAN is 1.0.
      pure $ T.isInfixOf "func.func @is_adult(" mlir
          && T.isInfixOf "func.func @is_adult$trace(" mlir
          && T.isInfixOf "@__l4_trace_enter" mlir
          && T.isInfixOf "@__l4_trace_exit"  mlir

-- | M5 slice 4A: AND/OR (and direct App "__AND__"/"__OR__") nodes get
-- a @"special"@ marker in the schema so the runtime knows to append a
-- synthetic IF sub-tree and apply short-circuit filtering. This is the
-- mechanism that flips `is-eligible` to byte-identical.
testTraceSpecialMarkers :: IO Bool
testTraceSpecialMarkers = do
  let src = T.unlines
        [ "@export Both"
        , "GIVEN `age` IS A NUMBER"
        , "GIVETH A BOOLEAN"
        , "DECIDE `ok` IS `age` >= 18 AND `age` <= 65"
        ]
  case schemaWithDiagnostics src of
    Left errs -> do
      putStrLn $ "\n    typecheck failed: " <> show errs
      pure False
    Right json ->
      pure $ T.isInfixOf "\"special\":\"AND\"" json

-- | An @MEANS@ / @DECIDE … IS@ binding without an explicit @GIVETH@
-- must still surface a concrete 'returnType' in the schema; otherwise
-- the runtime unmarshals NUMBER results as raw f64 (rational handles)
-- and a call like @squared 1@ comes back as @5e-324@ rather than @1@.
-- 'bundleExports' threads the typechecker's 'EntityInfo' so
-- 'enrichReturnTypes' can fill in the missing GIVETH.
testReturnTypeEnrichedFromEntityInfo :: IO Bool
testReturnTypeEnrichedFromEntityInfo = do
  let src = T.unlines
        [ "@export default Cube of a number"
        , "GIVEN n IS A NUMBER"
        , "cubed n MEANS n * n * n"
        ]
  case schemaWithDiagnostics src of
    Left errs -> do
      putStrLn $ "\n    typecheck failed: " <> show errs
      pure False
    Right json -> do
      let ok = T.isInfixOf "\"returnType\":\"NUMBER\"" json
      if ok then pure ()
            else putStrLn $ "\n    missing NUMBER returnType. got:\n    " <> T.unpack json
      pure ok

-- | M5 slice 4T: a @NOT P@ expression and its inner @P@ may collapse
-- into the same 'SrcRange' at parse time. The rangeMap is now keyed by
-- '(SrcRange, exprDisambiguator)', so both get distinct trace nodes
-- and the wrapper can look up each by its own AST shape. Verify the
-- schema has BOTH a NOT-tagged node AND a separate PROJ-tagged node
-- for an expression like @NOT req's flag@.
testTraceNotRangeDisambiguation :: IO Bool
testTraceNotRangeDisambiguation = do
  let src = T.unlines
        [ "DECLARE LoanRequest HAS"
        , "    `flag` IS A BOOLEAN"
        , ""
        , "@export Check"
        , "GIVEN req IS A LoanRequest"
        , "GIVETH A BOOLEAN"
        , "DECIDE `ok` IS NOT req's `flag`"
        ]
  case schemaWithDiagnostics src of
    Left errs -> do
      putStrLn $ "\n    typecheck failed: " <> show errs
      pure False
    Right json ->
      -- The NOT subtree must exist as one node; the inner PROJ subtree
      -- as another. If the rangeMap collapsed them, the PROJ node would
      -- be missing from the schema (its 'tnSpecial' would be absent).
      pure $ T.isInfixOf "\"special\":\"NOT\"" json
          && T.isInfixOf "\"special\":\"PROJ\"" json

-- | M5 slice 4A: every `<fn>$trace` clone calls @__l4_trace_enter_fn@
-- (with the fn's wasm symbol) at entry and @__l4_trace_exit_fn@ at
-- return, AND emits a fn-value @__l4_trace_enter/exit@ pair so the
-- runtime sees [fn-value, body] frames per call. Without this, nested
-- calls would resolve node IDs against the WRONG function's metadata.
testTraceFnValueAndContext :: IO Bool
testTraceFnValueAndContext = do
  let src = T.unlines
        [ "@export Adult check"
        , "GIVEN `age` IS A NUMBER"
        , "GIVETH A BOOLEAN"
        , "DECIDE `is_adult` IF `age` >= 18"
        ]
  case lowerSource src of
    Left errs -> do
      putStrLn $ "\n    typecheck failed: " <> show errs
      pure False
    Right mlir ->
      pure $ T.isInfixOf "func.func @is_adult$trace(" mlir
          && T.isInfixOf "@__l4_trace_enter_fn" mlir
          && T.isInfixOf "@__l4_trace_exit_fn"  mlir

-- | A plain (non-regulative) function has no state graph, so the schema
-- carries an empty stateGraphs array. (M3 negative control)
testNoStateGraphForPlainFn :: IO Bool
testNoStateGraphForPlainFn = do
  let src = T.unlines
        [ "@export Adult check"
        , "GIVEN `age` IS A NUMBER"
        , "GIVETH A BOOLEAN"
        , "DECIDE `is_adult` IF `age` >= 18"
        ]
  case checkWithImports emptyVFS src of
    Left errs -> do
      putStrLn $ "\n    typecheck failed: " <> show errs
      pure False
    Right r -> do
      let bundle = bundleExports "test.wasm" "test" r.tcdInfoMap (substitutedEntityInfo r) r.tcdModule []
          json = TE.decodeUtf8 (LBS.toStrict (encodeBundle bundle))
      pure $ T.isInfixOf "\"stateGraphs\":[]" json

-- | Verify that an @export-decorated DECIDE with a referenced ASSUME
-- emits a function whose argument count matches GIVEN + ASSUME.
testExportAssumePromotion :: IO Bool
testExportAssumePromotion = do
  let src = T.unlines
        [ "ASSUME `age` IS A NUMBER"
        , ""
        , "@export Check adult status"
        , "GIVEN `threshold` IS A NUMBER"
        , "GIVETH A BOOLEAN"
        , "DECIDE `is_adult` IF `age` >= `threshold`"
        ]
  case lowerSource src of
    Left errs -> do
      putStrLn $ "\n    typecheck failed: " <> show errs
      pure False
    Right mlir ->
      -- Exported function must take two f64 args (threshold + age).
      pure $ T.isInfixOf "func.func @is_adult(%0: f64, %1: f64)" mlir
          || T.isInfixOf "func.func @is_adult(%arg0: f64, %arg1: f64)" mlir

-- | When an internal (non-@export) function calls an @export function
-- that has ASSUME-derived args, the call site should invoke the ASSUME's
-- extern to fill the extra arg rather than pass too few.
testInternalCallFillsAssumes :: IO Bool
testInternalCallFillsAssumes = do
  let src = T.unlines
        [ "ASSUME `age` IS A NUMBER"
        , ""
        , "@export Core rule"
        , "GIVEN `threshold` IS A NUMBER"
        , "GIVETH A BOOLEAN"
        , "DECIDE `is_adult` IF `age` >= `threshold`"
        , ""
        , "GIVETH A BOOLEAN"
        , "DECIDE `adult_at_18` IF `is_adult` OF 18"
        ]
  case lowerSource src of
    Left errs -> do
      putStrLn $ "\n    typecheck failed: " <> show errs
      pure False
    Right mlir ->
      -- adult_at_18 must call the @age extern AND call is_adult with two args.
      pure $ T.isInfixOf "func.call @age()" mlir
          && T.isInfixOf "func.call @is_adult" mlir
      && Text.isInfixOf "arith.cmpf" mlir

-- | When the @export-ed function's parameter type is DECLAREd in an
-- IMPORTed module (not in the main file), the schema must still expand
-- the record's fields. Regression test for the ASEAN Cosmetic Directive
-- case where Information.l4 imports Declarations.l4 and the schema came
-- out as bare {"type":"object"}.
testSchemaUsesImportedDeclares :: IO Bool
testSchemaUsesImportedDeclares = do
  let declarations = T.unlines
        [ "DECLARE Widget HAS"
        , "    `serial`  IS A STRING"
        , "    `weight`  IS A NUMBER"
        ]
      info = T.unlines
        [ "IMPORT declarations"
        , ""
        , "@export Inspect a widget"
        , "GIVEN w IS A Widget"
        , "GIVETH A BOOLEAN"
        , "DECIDE `widget ok` IS w's weight > 0"
        ]
      vfs = vfsFromList [("declarations", declarations)]
  case checkWithImportsAndUri vfs "info" info of
    Left errs -> do
      putStrLn $ "\n    typecheck failed: " <> show errs
      pure False
    Right tc -> do
      let mainMod = tc.tcdModule
          depMods = map (\ri -> ri.riTypeChecked.program) tc.tcdResolvedImports
          bundle  = bundleExports "info.wasm" "test" tc.tcdInfoMap (substitutedEntityInfo tc) mainMod depMods
          json    = TE.decodeUtf8 (LBS.toStrict (encodeBundle bundle))
      -- The two field names must appear in the schema — they only show
      -- up if the imported DECLARE was reachable from typeToParameter.
      let ok = T.isInfixOf "\"serial\"" json
            && T.isInfixOf "\"weight\"" json
            && T.isInfixOf "\"propertyOrder\"" json
      unless ok $
        putStrLn $ "\n    schema missing imported fields. got:\n    " <> T.unpack json
      pure ok
  where
    unless b act = if b then pure () else act
