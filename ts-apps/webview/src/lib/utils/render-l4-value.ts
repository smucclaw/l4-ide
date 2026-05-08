import type { FunctionParameter } from 'jl4-client-rpc'

/**
 * Render a JSON value as L4 source syntax, using `x-l4-type` annotations
 * on the matching schema node to recover record / enum names.
 *
 * Indentation is driven directly by the structural depth of the value
 * — each nested record opens a new `WITH` block at the parent's
 * indent + 2 spaces, and every field lives on its own line. This
 * matches the visual shape of the Inspector's `#EVAL` formatter for
 * flat records, and stays correct for arbitrary nesting (the
 * inspector's regex pass over a flattened L4 string can't tell where
 * a nested record ends, which over-indents anything past it).
 *
 * Examples:
 *   { Person: { name: "Alice", age: 30 } }   + schema with x-l4-type "Person"
 *     → "Person WITH\n  name IS \"Alice\"\n  age IS 30"
 *   ["a", "b"]                               + items schema
 *     → "LIST \"a\", \"b\""
 *   "Alice"                                  + primitive string
 *     → '"Alice"'
 *   true / false                             + primitive boolean
 *     → "TRUE" / "FALSE"
 *
 * Falls back to JSON.stringify if the schema is missing or shape
 * mismatches the value.
 */
export function renderJsonAsL4(
  value: unknown,
  schema?: FunctionParameter
): string {
  return formatNode(value, schema, 0)
}

/**
 * Render the top-level *arguments* object of a tool call. The schema
 * for the args is a `FunctionParameters`-shaped node (type: object,
 * properties, required), but it has no L4 type — the args themselves
 * aren't a record. Each named arg is emitted as `name IS <value>`
 * on its own line, with record-typed argument values opening a
 * nested indented block (matching the value side).
 */
export function renderArgumentsAsL4(
  args: Record<string, unknown>,
  argsSchema: FunctionParameter
): string {
  const props = argsSchema.properties ?? {}
  const order = argsSchema.propertyOrder ?? Object.keys(props)
  const seen = new Set<string>()
  const lines: string[] = []
  for (const k of order) {
    if (!(k in args)) continue
    seen.add(k)
    lines.push(`${k} IS ${formatNode(args[k], props[k], 0)}`)
  }
  for (const k of Object.keys(args)) {
    if (seen.has(k)) continue
    lines.push(`${k} IS ${formatNode(args[k], props[k], 0)}`)
  }
  return lines.join('\n')
}

/**
 * Re-indent an L4 source string by folding parenthesis nesting into
 * indentation and breaking on commas / `WITH`. Mirrors the Inspector
 * panel's local formatter so a pre-rendered L4 string from the server
 * (e.g. an enum or scalar result) renders consistently with the
 * structural renderer above.
 *
 *   `Person WITH name IS "Alice", age IS 30`
 *     → "Person WITH\n  name IS \"Alice\"\n  age IS 30"
 *
 * Note: nested records WITHOUT delimiting parens drift right and
 * never come back — the inspector formatter has no way to detect
 * the close of an inner record. For schema-known values prefer
 * {@link renderJsonAsL4}, which uses tree depth directly.
 */
export function formatL4Indentation(text: string): string {
  let out = ''
  let depth = 0
  const indent = (): string => '\n' + '  '.repeat(depth)
  for (let i = 0; i < text.length; i++) {
    const ch = text[i]
    if (ch === '(') {
      depth++
    } else if (ch === ')') {
      depth = Math.max(0, depth - 1)
    } else if (ch === ',') {
      if (text[i + 1] === ' ') i++
      out += indent()
    } else if (text.startsWith(' WITH ', i)) {
      depth++
      out += ' WITH' + indent()
      i += 5
    } else {
      out += ch
    }
  }
  return out
}

function formatNode(
  value: unknown,
  schema: FunctionParameter | undefined,
  depth: number
): string {
  if (value === null || value === undefined) return 'NOTHING'
  if (typeof value === 'boolean') return value ? 'TRUE' : 'FALSE'
  if (typeof value === 'number') return String(value)
  if (typeof value === 'string') return formatString(value, schema)
  if (Array.isArray(value)) return formatArray(value, schema?.items, depth)
  if (typeof value === 'object') {
    return formatObject(value as Record<string, unknown>, schema, depth)
  }
  return JSON.stringify(value)
}

function formatString(value: string, schema?: FunctionParameter): string {
  // Enum constructors render bare; non-enum strings are quoted.
  if (schema?.enum && schema.enum.length > 0 && schema.enum.includes(value)) {
    return value
  }
  return `"${escapeL4String(value)}"`
}

function escapeL4String(s: string): string {
  return s.replace(/\\/g, '\\\\').replace(/"/g, '\\"')
}

function formatArray(
  value: unknown[],
  itemsSchema: FunctionParameter | undefined,
  depth: number
): string {
  if (value.length === 0) return 'LIST'
  const parts = value.map((v) => formatNode(v, itemsSchema, depth + 1))
  return `LIST ${parts.join(', ')}`
}

function formatObject(
  value: Record<string, unknown>,
  schema: FunctionParameter | undefined,
  depth: number
): string {
  const typeName = schema?.['x-l4-type']

  // Constructor-wrapped form: the L4 evaluator emits record values as
  // `{ TypeName: { ...fields } }`, nullary constructors as `{ Ctor: [] }`,
  // and unary constructors as `{ Ctor: [arg] }`. Peel the wrapper when
  // it matches the schema's `x-l4-type` (so we can render fields in
  // declared order); fall through to a single-key constructor form when
  // there's no schema at all so unknown wrapped values still look like
  // L4 instead of `{ Ctor: ... }`.
  const keys = Object.keys(value)
  if (keys.length === 1) {
    const ctor = keys[0]!
    const inner = value[ctor]
    const wrapperMatches = typeName === ctor
    if (wrapperMatches || !typeName) {
      if (Array.isArray(inner)) {
        if (inner.length === 0) return ctor
        if (inner.length === 1) {
          return `${ctor} ${formatNode(inner[0], undefined, depth + 1)}`
        }
        return `${ctor} ${formatArray(inner, undefined, depth)}`
      }
      if (inner !== null && typeof inner === 'object') {
        return formatObject(
          inner as Record<string, unknown>,
          wrapperMatches ? schema : undefined,
          depth
        )
      }
      return `${ctor} ${formatNode(inner, undefined, depth + 1)}`
    }
  }

  const props = schema?.properties
  // No L4 type info on a multi-field object → fall back to JSON. L4
  // has no anonymous-record syntax, so inventing one would mislead.
  if (!typeName || !props) return JSON.stringify(value, null, 2)

  // Field rendering follows the schema's declared order, with any
  // out-of-band keys appended so unexpected payload still surfaces.
  const order = schema?.propertyOrder ?? Object.keys(props)
  const seen = new Set<string>()
  const fields: string[] = []
  for (const k of order) {
    if (!(k in value)) continue
    seen.add(k)
    fields.push(`${k} IS ${formatNode(value[k], props[k], depth + 1)}`)
  }
  for (const k of Object.keys(value)) {
    if (seen.has(k)) continue
    fields.push(`${k} IS ${formatNode(value[k], props[k], depth + 1)}`)
  }
  if (fields.length === 0) return typeName

  // Inspector-style indentation, but driven by tree depth (not regex
  // over a flattened string), so nested records are correctly
  // dedented when they end. Each field at this record's level lives
  // on its own line at indent = (depth + 1) * 2 spaces.
  const indent = '  '.repeat(depth + 1)
  return `${typeName} WITH\n${fields.map((f) => indent + f).join('\n')}`
}
