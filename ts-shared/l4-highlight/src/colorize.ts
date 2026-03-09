/**
 * Lightweight L4 syntax colorizer — produces HTML with `tok-*` CSS classes.
 *
 * Designed for use in inspector panels and other contexts where the LSP
 * semantic tokens are not available. Covers the lexical categories that
 * appear in pretty-printed evaluator output.
 */

import {
  L4_KEYWORDS,
  L4_DIRECTIVES,
  L4_OPERATORS,
  type L4TokenType,
} from './tokens.js'

type PatternType = L4TokenType | 'kwOrIdentifier'

const TOKEN_PATTERNS: [RegExp, PatternType][] = [
  // Line comments (-- to end of line)
  [/^--[^\n]*/, 'comment'],
  // Directives (#EVAL, #ASSERT, etc.)
  [/^#[A-Z]+/, 'directive'],
  // String literals
  [/^"[^"]*"/, 'string'],
  // Backtick-quoted identifiers
  [/^`[^`]*`/, 'variable'],
  // Rational and integer numbers (including negative)
  [/^-?\d+(?:\.\d+)?/, 'number'],
  // Operators (try longer ones first via regex alternation)
  [/^(?:>=|<=|==|!=|&&|\|\||=>|[+\-*/<>=])/, 'operator'],
  // Identifiers and keywords (starts with letter, may contain digits and _)
  [/^[a-zA-Z][a-zA-Z0-9_]*/, 'kwOrIdentifier'],
]

function escapeHtml(text: string): string {
  return text.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
}

function classifyWord(word: string): L4TokenType {
  if (L4_KEYWORDS.has(word)) return 'keyword'
  if (L4_DIRECTIVES.has(word)) return 'directive'
  return 'identifier'
}

/**
 * Tokenize L4 source text into HTML with `tok-{type}` CSS class spans.
 *
 * Consumers provide their own CSS for .tok-keyword, .tok-directive, etc.
 */
export function colorize(text: string): string {
  let out = ''
  let i = 0
  while (i < text.length) {
    let matched = false
    for (const [pattern, rawType] of TOKEN_PATTERNS) {
      const m = pattern.exec(text.slice(i))
      if (m) {
        const t = m[0]
        const type: L4TokenType =
          rawType === 'kwOrIdentifier' ? classifyWord(t) : rawType
        out += `<span class="tok-${type}">${escapeHtml(t)}</span>`
        i += t.length
        matched = true
        break
      }
    }
    if (!matched) {
      // Check for operator characters that weren't caught by the regex
      const ch = text[i]
      if (L4_OPERATORS.has(ch)) {
        out += `<span class="tok-operator">${escapeHtml(ch)}</span>`
      } else {
        out += escapeHtml(ch)
      }
      i++
    }
  }
  return out
}

export { escapeHtml }
