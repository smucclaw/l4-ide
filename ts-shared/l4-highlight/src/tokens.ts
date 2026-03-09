/**
 * L4 token definitions — single source of truth for the TypeScript side.
 *
 * Canonical source: jl4-core/src/L4/Lexer.hs
 * Keep this file in sync when keywords or directives change in the Lexer.
 */

/**
 * Token categories, aligned with the Lexer's TokenCategory / LSP SemanticTokenTypes.
 *
 *   keyword    → CKeyword    → LSP Keyword
 *   directive  → CDirective  → LSP Macro
 *   annotation → CAnnotation → LSP Decorator
 *   comment    → CComment    → LSP Comment
 *   string     → CStringLit  → LSP String
 *   number     → CNumberLit  → LSP Number
 *   operator   → COperator   → LSP Operator
 *   symbol     → CSymbol     → LSP Operator
 *   variable   → CIdentifier (backtick-quoted)  → LSP Variable
 *   identifier → CIdentifier (regular, context may override to Type/Function/etc.)
 */
export type L4TokenType =
  | 'keyword'
  | 'directive'
  | 'annotation'
  | 'comment'
  | 'string'
  | 'number'
  | 'operator'
  | 'symbol'
  | 'variable'
  | 'identifier'

/**
 * Complete keyword list from jl4-core/src/L4/Lexer.hs `keywords` map,
 * plus built-in constants (TRUE, FALSE) that the parser handles specially.
 */
export const L4_KEYWORDS: ReadonlySet<string> = new Set([
  'A',
  'AKA',
  'ALL',
  'AN',
  'AND',
  'AS',
  'ASSUME',
  'AT',
  'BE',
  'BECAUSE',
  'BELOW',
  'ABOVE',
  'BRANCH',
  'BREACH',
  'BY',
  'CONCAT',
  'CONSIDER',
  'DECIDE',
  'DECLARE',
  'DIVIDED',
  'DO',
  'DOES',
  'ELSE',
  'ENV',
  'EQUALS',
  'EXACTLY',
  'FETCH',
  'FOLLOWED',
  'FOR',
  'FROM',
  'FUNCTION',
  'GIVEN',
  'GIVETH',
  'GIVES',
  'GREATER',
  'HAS',
  'HENCE',
  'IF',
  'IMPLIES',
  'IMPORT',
  'IN',
  'IS',
  'LEAST',
  'LESS',
  'LEST',
  'LET',
  'LIST',
  'MAY',
  'MEAN',
  'MEANS',
  'MINUS',
  'MODULO',
  'MOST',
  'MUST',
  'NOT',
  'OF',
  'ONE',
  'OR',
  'OTHERWISE',
  'PARTY',
  'PLUS',
  'POST',
  'PROVIDED',
  'RAND',
  'ROR',
  'SHANT',
  'STARTING',
  'THAN',
  'THE',
  'THEN',
  'TIMES',
  'TO',
  'TYPE',
  'UNLESS',
  'WHEN',
  'WHERE',
  'WITH',
  'WITHIN',
  'YIELD',
])

/**
 * Directive names (without the leading #).
 * From jl4-core/src/L4/Lexer.hs `directives` map.
 */
export const L4_DIRECTIVES: ReadonlySet<string> = new Set([
  'EVAL',
  'EVALTRACE',
  'CHECK',
  'TRACE',
  'ASSERT',
])

/**
 * Operator symbols from jl4-core/src/L4/Lexer.hs `operators` map.
 */
export const L4_OPERATORS: ReadonlySet<string> = new Set([
  '*',
  '+',
  '-',
  '>=',
  '<=',
  '>',
  '<',
  '=',
  '==',
  '!=',
  '&&',
  '||',
  '=>',
  '/',
])
