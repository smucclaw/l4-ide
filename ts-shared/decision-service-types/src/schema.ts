export type Parameter = {
  type: string
  alias: string | null
  enum: string[]
  description: string
  properties?: Record<string, Parameter>
  propertyOrder?: string[]
  items?: Parameter
}

export type Parameters = {
  type: 'object'
  properties: Record<string, Parameter>
  required: string[]
}

type SchemaSummaryOptions = {
  maxDepth?: number
  maxEnumValues?: number
  maxRecordKeys?: number
}

function baseTypeLabel(schemaType: string): string {
  switch (schemaType) {
    case 'boolean':
      return 'Bool'
    case 'number':
      return 'Number'
    case 'string':
      return 'Text'
    case 'object':
      return 'Record'
    case 'array':
      return 'List'
    default:
      return schemaType
  }
}

export function schemaSummary(
  schema: Parameter | null | undefined,
  opts: SchemaSummaryOptions = {}
): string | null {
  if (!schema) return null
  const maxDepth = opts.maxDepth ?? 2
  const maxEnumValues = opts.maxEnumValues ?? 4
  const maxRecordKeys = opts.maxRecordKeys ?? 4

  if (schema.alias) return schema.alias

  if (schema.enum && schema.enum.length > 0) {
    if (schema.enum.length <= maxEnumValues) {
      return `enum{${schema.enum.join('|')}}`
    }
    return `enum(${schema.enum.length})`
  }

  if (schema.items) {
    if (maxDepth <= 0) return 'List'
    const inner = schemaSummary(schema.items, {
      ...opts,
      maxDepth: maxDepth - 1,
    })
    return inner ? `List<${inner}>` : 'List'
  }

  if (schema.properties) {
    const keys =
      schema.propertyOrder ??
      Object.keys(schema.properties).sort((a, b) => a.localeCompare(b))
    if (keys.length === 0) return 'Record'
    if (maxDepth <= 0) return 'Record'
    const shown = keys.slice(0, maxRecordKeys)
    const more = keys.length > shown.length ? ',…' : ''
    return `Record{${shown.join(',')}${more}}`
  }

  return baseTypeLabel(schema.type) || null
}
