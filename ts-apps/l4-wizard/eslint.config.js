import { config } from '@repo/eslint-config/svelte'

export default [
  ...config,
  {
    ignores: ['*.mjs'],
  },
  {
    // Define global constants injected by Vite
    languageOptions: {
      globals: {
        __COMMIT_HASH__: 'readonly',
      },
    },
  },
]
