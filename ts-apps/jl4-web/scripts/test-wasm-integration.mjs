#!/usr/bin/env node
/**
 * Test script for L4 WASM integration
 *
 * This tests the WASM bridge outside of the browser to verify
 * the core functionality works correctly.
 *
 * Run with: node scripts/test-wasm-integration.mjs
 */

import { readFileSync } from 'fs'
import { fileURLToPath } from 'url'
import { dirname, join } from 'path'

const __dirname = dirname(fileURLToPath(import.meta.url))
const WASM_PATH = join(__dirname, '../static/wasm/jl4-core.wasm')
const JS_PATH = join(__dirname, '../static/wasm/jl4-core.mjs')

console.log('🧪 L4 WASM Integration Test\n')

// Test helper
function test(name, fn) {
  return async () => {
    try {
      await fn()
      console.log(`✅ ${name}`)
      return true
    } catch (e) {
      console.error(`❌ ${name}`)
      console.error(`   Error: ${e.message}`)
      return false
    }
  }
}

// Load JS FFI module
console.log('Loading JS FFI module...')
const jsModule = await import(JS_PATH)
const generateImports = jsModule.default
console.log('JS FFI module loaded\n')

// Load WASM
console.log('Loading WASM module...')
const wasmBuffer = readFileSync(WASM_PATH)
console.log(`WASM size: ${(wasmBuffer.length / 1024 / 1024).toFixed(1)} MB`)

const wasmModule = await WebAssembly.compile(wasmBuffer)
console.log('WASM compiled\n')

// Create exports proxy
const exportsProxy = {}

// Generate JS FFI imports
const jsFFIImports = generateImports(exportsProxy)

// WASI imports
const wasiImports = {
  clock_time_get: (_clockId, _precision, out) => {
    const view = new DataView(exportsProxy.memory.buffer)
    view.setBigUint64(out, BigInt(Date.now()) * BigInt(1_000_000), true)
    return 0
  },
  fd_write: (fd, iovs, iovsLen, nwritten) => {
    if (fd === 1 || fd === 2) {
      const view = new DataView(exportsProxy.memory.buffer)
      let totalWritten = 0
      for (let i = 0; i < iovsLen; i++) {
        // Skip ptr (4 bytes), we only need len to calculate totalWritten
        const len = view.getUint32(iovs + i * 8 + 4, true)
        totalWritten += len
      }
      view.setUint32(nwritten, totalWritten, true)
    }
    return 0
  },
  fd_read: () => 0,
  fd_close: () => 0,
  fd_seek: () => 0,
  fd_fdstat_get: () => 0,
  fd_fdstat_set_flags: () => 0,
  fd_filestat_get: () => 0,
  fd_filestat_set_size: () => 0,
  fd_prestat_get: () => 8,
  fd_prestat_dir_name: () => 8,
  path_create_directory: () => 63,
  path_filestat_get: () => 63,
  path_open: () => 63,
  poll_oneoff: () => 0,
  proc_exit: (code) => {
    console.warn('proc_exit:', code)
  },
  args_sizes_get: (argc, argvBufSize) => {
    const view = new DataView(exportsProxy.memory.buffer)
    view.setUint32(argc, 0, true)
    view.setUint32(argvBufSize, 0, true)
    return 0
  },
  args_get: () => 0,
  environ_sizes_get: (environc, environBufSize) => {
    const view = new DataView(exportsProxy.memory.buffer)
    view.setUint32(environc, 0, true)
    view.setUint32(environBufSize, 0, true)
    return 0
  },
  environ_get: () => 0,
}

// Instantiate
console.log('Instantiating WASM...')
const instance = await WebAssembly.instantiate(wasmModule, {
  wasi_snapshot_preview1: wasiImports,
  ghc_wasm_jsffi: jsFFIImports,
})

// Copy exports to proxy
Object.assign(exportsProxy, instance.exports)

// Initialize RTS
console.log('Initializing Haskell RTS...')
instance.exports._initialize()
console.log('RTS initialized\n')

const exports = instance.exports

// Run tests
console.log('Running tests...\n')

const tests = [
  test('l4_check with valid code returns empty diagnostics', async () => {
    const result = await exports.l4_check('DECIDE x IS 42')
    const diagnostics = JSON.parse(result)
    if (!Array.isArray(diagnostics)) throw new Error('Expected array')
    if (diagnostics.length !== 0)
      throw new Error(`Expected 0 diagnostics, got ${diagnostics.length}`)
  }),

  test('l4_check with invalid code returns diagnostics', async () => {
    const result = await exports.l4_check('DECID x IS 42')
    const diagnostics = JSON.parse(result)
    if (!Array.isArray(diagnostics)) throw new Error('Expected array')
    if (diagnostics.length === 0)
      throw new Error('Expected diagnostics for invalid code')
  }),

  test('l4_hover returns hover info', async () => {
    const result = await exports.l4_hover('DECIDE x IS 42', 0, 7)
    const hover = JSON.parse(result)
    // Hover might be null for some positions, but should be valid JSON
    if (hover !== null && typeof hover !== 'object') {
      throw new Error('Expected object or null')
    }
  }),

  test('l4_completions returns completion items', async () => {
    const result = await exports.l4_completions('', 0, 0)
    const completions = JSON.parse(result)
    if (!Array.isArray(completions)) throw new Error('Expected array')
    // Should have some keyword completions
    if (completions.length === 0) throw new Error('Expected some completions')
  }),

  test('l4_semantic_tokens returns token data', async () => {
    const result = await exports.l4_semantic_tokens('DECIDE x IS 42')
    const tokens = JSON.parse(result)
    if (!tokens || typeof tokens !== 'object')
      throw new Error('Expected object')
    if (!Array.isArray(tokens.data)) throw new Error('Expected data array')
  }),

  test('l4_eval evaluates expressions', async () => {
    const result = await exports.l4_eval('#EVAL 1 + 2')
    const evalResult = JSON.parse(result)
    if (!evalResult || typeof evalResult !== 'object')
      throw new Error('Expected object')
  }),

  // === Import Resolution Tests ===

  test('IMPORT prelude resolves successfully', async () => {
    const code = `IMPORT prelude
DECIDE x IS sum (LIST 1, 2, 3)`
    const result = await exports.l4_check(code)
    const diagnostics = JSON.parse(result)
    if (!Array.isArray(diagnostics)) throw new Error('Expected array')
    // Should resolve without errors (prelude defines 'sum')
    const errors = diagnostics.filter((d) => d.severity === 1)
    if (errors.length > 0) {
      throw new Error(
        `Expected no errors, got: ${errors.map((e) => e.message).join(', ')}`
      )
    }
  }),

  test('IMPORT math resolves successfully (transitive prelude)', async () => {
    const code = `IMPORT math
DECIDE x IS exp 1`
    const result = await exports.l4_check(code)
    const diagnostics = JSON.parse(result)
    if (!Array.isArray(diagnostics)) throw new Error('Expected array')
    // Should resolve without errors (math defines 'exp')
    const errors = diagnostics.filter((d) => d.severity === 1)
    if (errors.length > 0) {
      throw new Error(
        `Expected no errors, got: ${errors.map((e) => e.message).join(', ')}`
      )
    }
  }),

  test('IMPORT nonexistent module returns error', async () => {
    const code = `IMPORT nonexistent_module_xyz
DECIDE x IS 42`
    const result = await exports.l4_check(code)
    const diagnostics = JSON.parse(result)
    if (!Array.isArray(diagnostics)) throw new Error('Expected array')
    // Should have an error about the missing module
    if (diagnostics.length === 0) {
      throw new Error('Expected error for nonexistent import')
    }
    const hasImportError = diagnostics.some(
      (d) =>
        d.message.toLowerCase().includes('import') ||
        d.message.toLowerCase().includes('module') ||
        d.message.toLowerCase().includes('not found')
    )
    if (!hasImportError) {
      throw new Error(
        `Expected import error, got: ${diagnostics.map((e) => e.message).join(', ')}`
      )
    }
  }),

  test('IMPORT prelude provides prelude functions for eval', async () => {
    const code = `IMPORT prelude
#EVAL sum (LIST 1, 2, 3, 4, 5)`
    const result = await exports.l4_eval(code)
    const evalResult = JSON.parse(result)
    if (!evalResult || typeof evalResult !== 'object')
      throw new Error('Expected object')
    // Should successfully evaluate to 15
    if (evalResult.success) {
      const results = evalResult.results
      if (!results || results.length === 0) {
        throw new Error('Expected evaluation results')
      }
      // Check if the result contains '15'
      const resultStr = results[0].result
      if (!resultStr.includes('15')) {
        throw new Error(`Expected result to be 15, got: ${resultStr}`)
      }
    }
  }),

  test('Multiple embedded libraries are available', async () => {
    // Test that several libraries can be imported
    const libraries = ['prelude', 'math', 'currency']
    for (const lib of libraries) {
      const code = `IMPORT ${lib}
DECIDE x IS 42`
      const result = await exports.l4_check(code)
      const diagnostics = JSON.parse(result)
      // Filter out non-error diagnostics
      const errors = diagnostics.filter((d) => d.severity === 1)
      // Should not have "module not found" type errors
      const importErrors = errors.filter(
        (d) =>
          d.message.toLowerCase().includes('import') ||
          d.message.toLowerCase().includes('not found')
      )
      if (importErrors.length > 0) {
        throw new Error(
          `Library '${lib}' failed to import: ${importErrors.map((e) => e.message).join(', ')}`
        )
      }
    }
  }),

  test('IMPORT daydate resolves and provides date functions', async () => {
    // Test daydate library - it defines date constants and functions
    const code = `IMPORT daydate
DECIDE myDay IS Monday
DECIDE daysInWeek IS \`Days in a week\``
    const result = await exports.l4_check(code)
    const diagnostics = JSON.parse(result)
    if (!Array.isArray(diagnostics)) throw new Error('Expected array')
    // Should resolve without errors
    const errors = diagnostics.filter((d) => d.severity === 1)
    if (errors.length > 0) {
      throw new Error(
        `Expected no errors, got: ${errors.map((e) => e.message).join(', ')}`
      )
    }
  }),

  test('IMPORT daydate eval works with date constants', async () => {
    // Test that daydate constants can be evaluated
    const code = `IMPORT daydate
#EVAL \`Days in a week\``
    const result = await exports.l4_eval(code)
    const evalResult = JSON.parse(result)
    if (!evalResult || typeof evalResult !== 'object')
      throw new Error('Expected object')
    if (evalResult.success) {
      const results = evalResult.results
      if (!results || results.length === 0) {
        throw new Error('Expected evaluation results')
      }
      // Days in a week should be 7
      const resultStr = results[0].result
      if (!resultStr.includes('7')) {
        throw new Error(`Expected result to contain 7, got: ${resultStr}`)
      }
    } else {
      // If not successful, show the diagnostics
      const diagMsgs = (evalResult.diagnostics || [])
        .map((d) => d.message)
        .join(', ')
      throw new Error(`Evaluation failed: ${diagMsgs}`)
    }
  }),
]

let passed = 0
let failed = 0

for (const t of tests) {
  if (await t()) {
    passed++
  } else {
    failed++
  }
}

console.log(`\n${'='.repeat(40)}`)
console.log(`Results: ${passed} passed, ${failed} failed`)
console.log('='.repeat(40))

process.exit(failed > 0 ? 1 : 0)
