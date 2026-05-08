import type { FunctionParameter } from 'jl4-client-rpc'

/**
 * Render a JSON value as L4 source syntax, using `x-l4-type` annotations
 * on the matching schema node to recover record / enum names.
 *
 * Indentation is driven directly by the structural depth of the value
 * — each nested record opens a new `WITH` block at the parent's
 * indent + 2 spaces, and every field lives on its own line.
 *
 * Field-name resolution bridges the gap between the LLM-facing
 * sanitised property names (hyphens replacing whitespace, applied by
 * jl4-service's MCP exporter) and the original spaced names that
 * the function-schema endpoint preserves: each child schema carries
 * an `x-sanitized-name` annotation when its key was sanitised, so we
 * can map a value-key like `event-type` back to the schema-key
 * `event type` without duplicating the server's sanitisation.
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
  for (const origKey of order) {
    const fieldSchema = props[origKey]
    const vk = findValueKey(origKey, fieldSchema, args)
    if (vk === null) continue
    seen.add(vk)
    lines.push(
      `${quoteIdent(origKey)} IS ${formatNode(args[vk], fieldSchema, 0)}`
    )
  }
  for (const k of Object.keys(args)) {
    if (seen.has(k)) continue
    const { originalKey, schema: fieldSchema } = lookupSchemaFor(props, k)
    lines.push(
      `${quoteIdent(originalKey)} IS ${formatNode(args[k], fieldSchema, 0)}`
    )
  }
  return lines.join('\n')
}

/**
 * Re-indent an L4 source string by folding parenthesis nesting into
 * indentation and breaking on commas / `WITH`. Mirrors the Inspector
 * panel's local formatter so a pre-rendered L4 string from the server
 * (e.g. an enum or scalar result) renders consistently with the
 * structural renderer above.
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
  // Multi-line when any item rendered onto multiple lines (records,
  // nested lists). Flat primitive lists stay inline.
  const anyMulti = parts.some((p) => p.includes('\n'))
  if (!anyMulti) return `LIST ${parts.join(', ')}`
  const indent = '  '.repeat(depth + 1)
  return `LIST\n${parts.map((p) => indent + indentContinuationLines(p, indent)).join(',\n')}`
}

/**
 * Indent every line after the first by the given prefix. Used when an
 * already-multiline child string is dropped into a list slot — each
 * subsequent line needs the same leading indent so the L4 block stays
 * vertically aligned under its bullet.
 */
function indentContinuationLines(s: string, indent: string): string {
  const [first, ...rest] = s.split('\n')
  if (rest.length === 0) return first ?? ''
  return [first, ...rest.map((line) => indent + line)].join('\n')
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
        if (inner.length === 0) return quoteIdent(ctor)
        if (inner.length === 1) {
          return `${quoteIdent(ctor)} ${formatNode(inner[0], undefined, depth + 1)}`
        }
        return `${quoteIdent(ctor)} ${formatArray(inner, undefined, depth)}`
      }
      if (inner !== null && typeof inner === 'object') {
        return formatObject(
          inner as Record<string, unknown>,
          wrapperMatches ? schema : undefined,
          depth
        )
      }
      return `${quoteIdent(ctor)} ${formatNode(inner, undefined, depth + 1)}`
    }
  }

  const props = schema?.properties
  // No L4 type info on a multi-field object → fall back to JSON. L4
  // has no anonymous-record syntax, so inventing one would mislead.
  if (!typeName || !props) return JSON.stringify(value, null, 2)

  // Field rendering follows the schema's declared order, then any
  // out-of-band keys append. Each child schema carries its
  // `x-sanitized-name` so a value-key from the LLM (sanitised form)
  // still resolves to the original schema key without duplicating
  // the server's sanitiser.
  const order = schema?.propertyOrder ?? Object.keys(props)
  const seen = new Set<string>()
  const fields: string[] = []
  for (const origKey of order) {
    const fieldSchema = props[origKey]
    const vk = findValueKey(origKey, fieldSchema, value)
    if (vk === null) continue
    seen.add(vk)
    fields.push(
      `${quoteIdent(origKey)} IS ${formatNode(value[vk], fieldSchema, depth + 1)}`
    )
  }
  for (const k of Object.keys(value)) {
    if (seen.has(k)) continue
    const { originalKey, schema: fieldSchema } = lookupSchemaFor(props, k)
    fields.push(
      `${quoteIdent(originalKey)} IS ${formatNode(value[k], fieldSchema, depth + 1)}`
    )
  }
  if (fields.length === 0) return quoteIdent(typeName)

  const indent = '  '.repeat(depth + 1)
  return `${quoteIdent(typeName)} WITH\n${fields.map((f) => indent + indentContinuationLines(f, indent)).join('\n')}`
}

/**
 * Backtick-wrap an identifier that contains whitespace, matching the
 * `quoteIdent` helper jl4-service uses when emitting record literals.
 * L4's parser requires backticks around multi-word identifiers; rendering
 * them without wrappers produces invalid L4.
 */
function quoteIdent(name: string): string {
  return /\s/.test(name) ? `\`${name}\`` : name
}

/**
 * Find which key in `value` corresponds to the schema's `origKey`.
 * Tries a direct match first, then the child schema's
 * `x-sanitized-name` annotation (which the server fills in when the
 * sanitised form differs from the original key).
 */
function findValueKey(
  origKey: string,
  fieldSchema: FunctionParameter | undefined,
  value: Record<string, unknown>
): string | null {
  if (origKey in value) return origKey
  const sanitised = fieldSchema?.['x-sanitized-name']
  if (sanitised && sanitised !== origKey && sanitised in value) return sanitised
  return null
}

/**
 * Reverse lookup — when iterating over the value's keys (which may
 * be the LLM's sanitised names), find the original schema key plus
 * its parameter node. Falls back to the value key itself with an
 * undefined schema when no match is found, so unrecognised payload
 * still surfaces with sensible labelling.
 */
function lookupSchemaFor(
  props: Record<string, FunctionParameter> | undefined,
  valueKey: string
): { originalKey: string; schema: FunctionParameter | undefined } {
  if (!props) return { originalKey: valueKey, schema: undefined }
  const direct = props[valueKey]
  if (direct) return { originalKey: valueKey, schema: direct }
  for (const [orig, p] of Object.entries(props)) {
    if (p['x-sanitized-name'] === valueKey) {
      return { originalKey: orig, schema: p }
    }
  }
  return { originalKey: valueKey, schema: undefined }
}
