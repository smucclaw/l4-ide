import type { Parameter, Parameters } from './schema.js'

export type InputRef = {
  rootUnique: number
  path: string[]
}

export type QueryAtom = {
  unique: number
  atomId: string
  label: string
  inputRefs?: InputRef[]
}

export type QueryOutcome = {
  determined: boolean | null
  support: QueryAtom[]
}

export type QueryImpact = {
  ifTrue: QueryOutcome
  ifFalse: QueryOutcome
}

export type QueryInput = {
  inputUnique: number
  inputLabel: string
  score: number
  atoms: QueryAtom[]
}

export type QueryAsk = {
  container: string
  key: string | null
  path: string[]
  label: string
  score: number
  atoms: QueryAtom[]
  schema: Parameter | null
}

export type LadderNode =
  | { $type: 'And'; id: { id: number }; args: LadderNode[] }
  | { $type: 'Or'; id: { id: number }; args: LadderNode[] }
  | { $type: 'Not'; id: { id: number }; negand: LadderNode }
  | {
      $type: 'UBoolVar'
      id: { id: number }
      name: { label: string; unique: number }
      value: string
      atomId: string
      canInline: boolean
    }
  | {
      $type: 'App'
      id: { id: number }
      fnName: { label: string; unique: number }
      args: LadderNode[]
      atomId: string
    }

export type Ladder = {
  funDecl: {
    $type: 'FunDecl'
    body: LadderNode
  }
} | null

export type QueryPlanResponse = {
  determined: boolean | null
  stillNeeded: QueryAtom[]
  ranked: QueryAtom[]
  inputs: QueryInput[]
  asks: QueryAsk[]
  impact: Record<string, QueryImpact>
  impactByAtomId: Record<string, QueryImpact>
  note: string
  ladder: Ladder
}

export function askKeyFromPath(path: string[]): string | null {
  return path.length === 0 ? null : path.join('.')
}

export function askLabelFromPath(container: string, path: string[]): string {
  return path.length === 0 ? container : `${container}.${path.join('.')}`
}

type ConsumePropertyResult = {
  key: string
  value: Parameter
  rest: string[]
}

function consumeProperty(
  properties: Record<string, Parameter>,
  segments: string[]
): ConsumePropertyResult | null {
  for (let i = segments.length; i >= 1; i -= 1) {
    const key = segments.slice(0, i).join('.')
    const value = properties[key]
    if (value) {
      return { key, value, rest: segments.slice(i) }
    }
  }
  return null
}

export function schemaAtPath(
  root: Parameter,
  segments: string[]
): Parameter | null {
  let current: Parameter | undefined = root
  let rest: string[] = segments

  while (current && rest.length > 0) {
    const head = rest[0]!
    const tail = rest.slice(1)

    if (current.items) {
      if (head === '*' || head === '[]') {
        current = current.items
        rest = tail
        continue
      }
      if (/^\d+$/.test(head)) {
        current = current.items
        rest = tail
        continue
      }
      return null
    }

    if (current.properties) {
      const consumed = consumeProperty(current.properties, rest)
      if (!consumed) return null
      current = consumed.value
      rest = consumed.rest
      continue
    }

    return null
  }

  return current ?? null
}

export function schemaAtPathVariants(
  root: Parameter,
  segments: string[]
): Parameter | null {
  const splitSegments = segments.flatMap((s) => s.split('.').filter(Boolean))
  if (splitSegments.join('\u0000') === segments.join('\u0000')) {
    return schemaAtPath(root, segments)
  }
  return schemaAtPath(root, segments) ?? schemaAtPath(root, splitSegments)
}

type PathSortKey =
  | { tag: 'field'; index: number; key: string }
  | { tag: 'index'; index: number }
  | { tag: 'wildcard' }
  | { tag: 'unknown'; key: string }

function comparePathSortKey(a: PathSortKey, b: PathSortKey): number {
  if (a.tag !== b.tag) return a.tag < b.tag ? -1 : 1
  switch (a.tag) {
    case 'field':
      if (b.tag !== 'field') return 0
      return a.index !== b.index
        ? a.index - b.index
        : a.key.localeCompare(b.key)
    case 'index':
      if (b.tag !== 'index') return 0
      return a.index - b.index
    case 'wildcard':
      return 0
    case 'unknown':
      if (b.tag !== 'unknown') return 0
      return a.key.localeCompare(b.key)
  }
}

function comparePathSortKeys(a: PathSortKey[], b: PathSortKey[]): number {
  const n = Math.max(a.length, b.length)
  for (let i = 0; i < n; i += 1) {
    const ai = a[i]
    const bi = b[i]
    if (!ai) return -1
    if (!bi) return 1
    const c = comparePathSortKey(ai, bi)
    if (c !== 0) return c
  }
  return 0
}

function fieldOrderIndex(schema: Parameter): Record<string, number> {
  const keys = schema.properties ? Object.keys(schema.properties).sort() : []
  const order = schema.propertyOrder ?? keys
  return Object.fromEntries(order.map((k, i) => [k, i]))
}

function pathSortKeyFromSchema(
  root: Parameter,
  segments: string[]
): PathSortKey[] {
  let current: Parameter | undefined = root
  let rest: string[] = segments
  const out: PathSortKey[] = []

  while (current && rest.length > 0) {
    const head = rest[0]!
    const tail = rest.slice(1)

    if (current.items) {
      if (head === '*' || head === '[]') {
        out.push({ tag: 'wildcard' })
        current = current.items
        rest = tail
        continue
      }
      if (/^\d+$/.test(head)) {
        out.push({ tag: 'index', index: Number(head) })
        current = current.items
        rest = tail
        continue
      }
      out.push({ tag: 'unknown', key: head })
      current = current.items
      rest = tail
      continue
    }

    if (current.properties) {
      const consumed = consumeProperty(current.properties, rest)
      if (!consumed) {
        out.push({ tag: 'unknown', key: head })
        rest = tail
        continue
      }

      const indexByKey = fieldOrderIndex(current)
      const index = indexByKey[consumed.key] ?? Number.MAX_SAFE_INTEGER

      out.push({ tag: 'field', index, key: consumed.key })
      current = consumed.value
      rest = consumed.rest
      continue
    }

    out.push({ tag: 'unknown', key: head })
    rest = tail
  }

  return out
}

export function sortAsksForElicitation(
  params: Parameters,
  asks: QueryAsk[]
): QueryAsk[] {
  const schemaFor = (ask: QueryAsk): Parameter | undefined =>
    params.properties[ask.container]

  return [...asks].sort((a, b) => {
    if (a.score !== b.score) return b.score - a.score
    if (a.container !== b.container)
      return a.container.localeCompare(b.container)

    const aSchema = schemaFor(a)
    const bSchema = schemaFor(b)

    const aKey = aSchema
      ? pathSortKeyFromSchema(aSchema, a.path)
      : a.path.map((s) => ({ tag: 'unknown', key: s }) as const)
    const bKey = bSchema
      ? pathSortKeyFromSchema(bSchema, b.path)
      : b.path.map((s) => ({ tag: 'unknown', key: s }) as const)

    const c = comparePathSortKeys(aKey, bKey)
    if (c !== 0) return c

    return a.label.localeCompare(b.label)
  })
}
