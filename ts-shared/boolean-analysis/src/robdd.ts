export type BddNodeId = number

type Op = 'and' | 'or'

type Node =
  | { readonly kind: 'terminal'; readonly value: 0 | 1 }
  | {
      readonly kind: 'node'
      readonly var: number
      readonly low: BddNodeId
      readonly high: BddNodeId
    }

type NodeKey = `${number},${number},${number}`
type ApplyKey = `${Op},${BddNodeId},${BddNodeId}`
type NegKey = `${BddNodeId}`
type RestrictKey = `${BddNodeId}|${string}`

function opEval(op: Op, a: 0 | 1, b: 0 | 1): 0 | 1 {
  if (op === 'and') return (a & b) as 0 | 1
  return (a | b) as 0 | 1
}

function bindingsKey(bindings: ReadonlyMap<number, boolean>): string {
  const pairs = Array.from(bindings.entries())
    .sort(([a], [b]) => a - b)
    .map(([v, b]) => `${v}=${b ? 1 : 0}`)
  return pairs.join(',')
}

export class ROBDD {
  static readonly FALSE: BddNodeId = 0
  static readonly TRUE: BddNodeId = 1

  readonly #nodes: Node[] = [
    { kind: 'terminal', value: 0 },
    { kind: 'terminal', value: 1 },
  ]

  readonly #uniqueTable = new Map<NodeKey, BddNodeId>()
  readonly #applyMemo = new Map<ApplyKey, BddNodeId>()
  readonly #negMemo = new Map<NegKey, BddNodeId>()
  readonly #restrictMemo = new Map<RestrictKey, BddNodeId>()

  mk(varIndex: number, low: BddNodeId, high: BddNodeId): BddNodeId {
    if (low === high) return low
    const key: NodeKey = `${varIndex},${low},${high}`
    const existing = this.#uniqueTable.get(key)
    if (existing !== undefined) return existing
    const id = this.#nodes.length
    this.#nodes.push({ kind: 'node', var: varIndex, low, high })
    this.#uniqueTable.set(key, id)
    return id
  }

  var(varIndex: number): BddNodeId {
    return this.mk(varIndex, ROBDD.FALSE, ROBDD.TRUE)
  }

  isTerminal(id: BddNodeId): boolean {
    return id === ROBDD.FALSE || id === ROBDD.TRUE
  }

  nodeCount(): number {
    return this.#nodes.length
  }

  private getNode(id: BddNodeId): Node {
    const n = this.#nodes[id]
    if (!n) throw new Error(`Internal error: missing BDD node ${id}`)
    return n
  }

  neg(id: BddNodeId): BddNodeId {
    const memoKey: NegKey = `${id}`
    const existing = this.#negMemo.get(memoKey)
    if (existing !== undefined) return existing

    const node = this.getNode(id)
    if (node.kind === 'terminal') {
      const res = node.value === 0 ? ROBDD.TRUE : ROBDD.FALSE
      this.#negMemo.set(memoKey, res)
      return res
    }

    const res = this.mk(node.var, this.neg(node.low), this.neg(node.high))
    this.#negMemo.set(memoKey, res)
    return res
  }

  apply(op: Op, a: BddNodeId, b: BddNodeId): BddNodeId {
    if (a === b) return a

    if (this.isTerminal(a) && this.isTerminal(b)) {
      const av = a === ROBDD.TRUE ? 1 : 0
      const bv = b === ROBDD.TRUE ? 1 : 0
      return opEval(op, av, bv) === 1 ? ROBDD.TRUE : ROBDD.FALSE
    }

    const key: ApplyKey = `${op},${a},${b}`
    const existing = this.#applyMemo.get(key)
    if (existing !== undefined) return existing

    const an = this.getNode(a)
    const bn = this.getNode(b)
    const aVar = an.kind === 'node' ? an.var : Number.POSITIVE_INFINITY
    const bVar = bn.kind === 'node' ? bn.var : Number.POSITIVE_INFINITY
    const top = Math.min(aVar, bVar)

    const [aLow, aHigh] =
      an.kind === 'node' && an.var === top ? [an.low, an.high] : [a, a]
    const [bLow, bHigh] =
      bn.kind === 'node' && bn.var === top ? [bn.low, bn.high] : [b, b]

    const low = this.apply(op, aLow, bLow)
    const high = this.apply(op, aHigh, bHigh)
    const res = this.mk(top, low, high)
    this.#applyMemo.set(key, res)
    return res
  }

  restrict(id: BddNodeId, bindings: ReadonlyMap<number, boolean>): BddNodeId {
    if (this.isTerminal(id) || bindings.size === 0) return id

    const memoKey: RestrictKey = `${id}|${bindingsKey(bindings)}`
    const existing = this.#restrictMemo.get(memoKey)
    if (existing !== undefined) return existing

    const node = this.getNode(id)
    if (node.kind === 'terminal') return id

    const bound = bindings.get(node.var)
    if (bound !== undefined) {
      const res = this.restrict(bound ? node.high : node.low, bindings)
      this.#restrictMemo.set(memoKey, res)
      return res
    }

    const low = this.restrict(node.low, bindings)
    const high = this.restrict(node.high, bindings)
    const res = this.mk(node.var, low, high)
    this.#restrictMemo.set(memoKey, res)
    return res
  }

  support(id: BddNodeId): ReadonlySet<number> {
    const vars = new Set<number>()
    const visited = new Set<BddNodeId>()

    const go = (nId: BddNodeId) => {
      if (this.isTerminal(nId) || visited.has(nId)) return
      visited.add(nId)
      const node = this.getNode(nId)
      if (node.kind !== 'node') return
      vars.add(node.var)
      go(node.low)
      go(node.high)
    }

    go(id)
    return vars
  }
}
