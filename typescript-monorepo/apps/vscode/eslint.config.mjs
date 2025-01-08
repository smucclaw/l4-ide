// @ts-check

import prettier from 'eslint-config-prettier'
import js from '@eslint/js'
import globals from 'globals'
import ts from 'typescript-eslint'

export default ts.config(
  js.configs.recommended,
  ...ts.configs.recommended,
  ...ts.configs.recommendedTypeChecked,
  prettier,
  {
    languageOptions: {
      globals: {
        ...globals.browser,
        ...globals.node,
      },
    },
  },
  {

    languageOptions: {
      parserOptions: {
        parser: ts.parser,
      },
    },
  },
  {
    ignores: ['build/', 'out/', 'dist/', 'eslint.config.mjs'],
  }
)


// export default tseslint.config(
//   eslint.configs.recommended,
//   tseslint.configs.recommended,
//   tseslint.configs.recommendedTypeChecked,
//   {
//     // specify custom warnings
//     rules: {
//       "@typescript-eslint/naming-convention": [
//         "warn",
//         {
//           selector: "default",
//           format: ["camelCase", "PascalCase"],
//         },
//       ],
//       curly: "warn",
//       eqeqeq: "warn",
//       "no-throw-literal": "warn",
//       semi: [2, "never"],
//       "@typescript-eslint/no-unused-vars": 0,
//       "@typescript-eslint/no-explicit-any": 0,
//       "@typescript-eslint/explicit-module-boundary-types": 0,
//       "@typescript-eslint/no-non-null-assertion": 0,
//     },
//   },
//   {
//     languageOptions: {
//       parser: tsParser,
//       parserOptions: {
//         projectService: true,
//         tsconfigRootDir: import.meta.dirname,
//       },
//       ecmaVersion: 2022,
//       sourceType: "module",
//     },
//   },
//   {
//     // disable typechecking for this file only
//     files: ["**/*.mjs"],
//     extends: [tseslint.configs.disableTypeChecked],
//   },
// )
