import ts from 'typescript-eslint'
import svelte from 'eslint-plugin-svelte'
import { config as baseConfig } from './base.js'

// Note that the base config already has the browser and node globals
export const config = [
  ...baseConfig,
  ...svelte.configs['flat/prettier'],
  {
    files: ['**/*.svelte'],

    languageOptions: {
      parserOptions: {
        parser: ts.parser,
      },
    },
  },
  {
    ignores: ['build/', '.svelte-kit/', 'dist/'],
  }
]