/**
 * Monaco Monarch tokenizer definition for L4, built from shared token data.
 *
 * Returns a plain object compatible with Monaco's IMonarchLanguage.
 * Does not import Monaco — safe to use in any environment.
 */

import { L4_KEYWORDS } from './tokens.js'

/**
 * Returns a Monarch language definition for the jl4 language.
 *
 * Usage with Monaco:
 *   monaco.languages.setMonarchTokensProvider('jl4', monarchTokensProvider())
 */
export function monarchTokensProvider(): object {
  return {
    keywords: [...L4_KEYWORDS],
    tokenizer: {
      root: [
        // Line comments
        [/--.*$/, 'comment'],
        // Directives (#EVAL, #ASSERT, etc.)
        [/#[A-Z]+/, 'directive'],
        // Annotations (@nlg, @ref, etc.)
        [/@[a-z][\w-]*/, 'annotation'],
        // String literals
        [/"[^"]*"/, 'string'],
        // Backtick-quoted identifiers
        [/`[^`]*`/, 'variable.name'],
        // Rational and integer numbers
        [/\d+(\.\d+)?/, 'number'],
        // Operators
        [/>=|<=|==|!=|&&|\|\||=>|[+\-*/<>=]/, 'operator'],
        // Identifiers and keywords: uppercase words checked against keyword list
        [
          /[A-Z][A-Za-z0-9_]*/,
          {
            cases: {
              '@keywords': 'keyword',
              '@default': 'type.identifier',
            },
          },
        ],
        // Regular identifiers (lowercase-starting)
        [/[a-z_][a-zA-Z0-9_]*/, 'identifier'],
        // Genitive
        [/'s\b/, 'keyword'],
      ],
    },
  }
}
