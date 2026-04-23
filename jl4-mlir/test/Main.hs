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
import L4.MLIR.Lower (lowerProgramWithInfo)
import L4.MLIR.Schema (bundleExports, encodeBundle)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as TE

import L4.API.VirtualFS (checkWithImports, checkWithImportsAndUri, emptyVFS, vfsFromList)
import L4.Import.Resolution (TypeCheckWithDepsResult(..), ResolvedImport(..))
import L4.TypeCheck.Types (CheckResult(..))

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
          bundle  = bundleExports "info.wasm" "test" mainMod depMods
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
