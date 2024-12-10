/*********************************************
       Registry
***********************************************/
export type LirRootType = string;

/** Lir := 'Layout IR' */
export class LirRegistry {
  #roots: Map<LirRootType, LirNode> = new Map()

  getRoot(rootType: LirRootType): LirNode | undefined {
    return this.#roots.get(rootType)
  }

  setRoot(rootType: LirRootType, node: LirNode) {
    this.#roots.set(rootType, node)
  }
}

export class LirId {
  private static counter = 0

  constructor(private id: symbol = Symbol(LirId.counter++)) {}

  toString() {
    return this.id.toString()
  }

  isEqualTo(other: LirId) {
    return this.id === other.id
  }
}

/*********************************************
       LirNode
***********************************************/

export interface LirNode {
  getId(): LirId

  getChildren(context: LirContext): LirNode[]

  toString(): string
}

export interface LirNodeInfo {
  context: LirContext
  registry: LirRegistry
}

export abstract class DefaultLirNode implements LirNode {
  #id: LirId
  #nodeInfo: Omit<LirNodeInfo, 'context'>

  constructor(protected readonly nodeInfo: LirNodeInfo) {
    this.#id = new LirId()
    this.#nodeInfo = { registry: nodeInfo.registry }
  }

  protected getLirRegistry() {
    return this.#nodeInfo.registry
  }

  getId(): LirId {
    return this.#id
  }

  abstract getChildren(context: LirContext): LirNode[]

  abstract toString(): string
}

/*********************************************
       LirContext
***********************************************/

export class LirContext {
  #nodes: Map<LirId, LirNode> = new Map()

  constructor() {}

  getNode(id: LirId) {
    return this.#nodes.get(id)
  }

  setNode(node: LirNode) {
    this.#nodes.set(node.getId(), node)
  }
}

/*********************************************
       LirDataSource
***********************************************/

export interface LirDataSource<A, B> {
  toLir(data: A): B
}
