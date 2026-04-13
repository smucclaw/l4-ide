-- | L4 → MLIR compiler backend targeting WebAssembly.
--
-- This is the top-level module for the jl4-mlir package. It re-exports
-- the public API for:
--
--   * Lowering L4 AST to MLIR IR ('lowerModule', 'lowerProgram')
--   * Emitting MLIR textual IR ('renderMLIR')
--   * Running the full compilation pipeline ('compileToPipeline', 'compileToWasm')
--
-- == Architecture
--
-- @
--   L4 source (.l4)
--     │
--     ▼  jl4-core: parse, resolve imports, typecheck
--   Module Resolved (typed AST)
--     │
--     ▼  L4.MLIR.Lower: walk AST, generate operations
--   MLIRModule (Haskell IR)
--     │
--     ▼  L4.MLIR.Emit: pretty-print to textual format
--   .mlir file
--     │
--     ▼  mlir-opt: --convert-scf-to-cf --convert-func-to-llvm ...
--   .opt.mlir (LLVM dialect)
--     │
--     ▼  mlir-translate: --mlir-to-llvmir
--   .ll file (LLVM IR)
--     │
--     ▼  llc -march=wasm32
--   .o file (wasm32 object)
--     │
--     ▼  wasm-ld
--   .wasm binary
-- @
--
-- == Type mapping
--
-- > L4 NUMBER   → f64
-- > L4 BOOLEAN  → i1
-- > L4 STRING   → !llvm.ptr (UTF-8)
-- > L4 Record   → !llvm.struct<"Name", (fields...)>
-- > L4 Enum     → i32 tag
-- > L4 MAYBE T  → !llvm.struct<"l4.maybe", (i1, T)>
-- > L4 LIST T   → !llvm.ptr (linked list node)
module L4.MLIR
  ( -- * Lowering
    lowerModule
  , lowerProgram
    -- * Emission
  , renderMLIR
    -- * Pipeline
  , compileToPipeline
  , compileToMLIR
  , compileToWasm
  , PipelineConfig (..)
  , OutputTarget (..)
  , defaultConfig
    -- * Re-exports for MLIR IR construction
  , MLIRModule (..)
  , Operation (..)
  , Value (..)
  , MLIRType (..)
  ) where

import L4.MLIR.IR (MLIRModule(..), Operation(..), Value(..), MLIRType(..))
import L4.MLIR.Emit (renderMLIR)
import L4.MLIR.Lower (lowerModule, lowerProgram)
import L4.MLIR.Pipeline (compileToPipeline, compileToMLIR, compileToWasm, PipelineConfig(..), OutputTarget(..), defaultConfig)
