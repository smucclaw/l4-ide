import { config } from '@repo/eslint-config/svelte'

export default [
  ...config,
  {
    ignores: [
      'static/wasm/*.mjs', // Exclude auto-generated WASM FFI glue code (from GHC WASM build)
    ],
  },
]
