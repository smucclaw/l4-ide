import { config } from '@repo/eslint-config/svelte'

export default [
  ...config,
  {
    ignores: ['*.mjs'],
  },
]
