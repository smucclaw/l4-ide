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
