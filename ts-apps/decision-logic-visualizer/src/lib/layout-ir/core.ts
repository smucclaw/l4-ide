/*********************************************
       Registry
***********************************************/
export type LirRootType = string

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
      NodeInfo
***********************************************/

type LirNodeInfoWithoutContext = Omit<LirNodeInfo, 'context'>

export interface LirNodeInfo {
  context: LirContext
  registry: LirRegistry
}

export abstract class NodeInfoManager {
  protected readonly lirInfo: LirNodeInfoWithoutContext

  /** Note: Make sure not to actually store the LirContext in the class. */
  constructor(defaultNodeInfo: LirNodeInfo) {
    const { context, ...lirInfoWithoutContext } = defaultNodeInfo
    this.lirInfo = lirInfoWithoutContext
  }

  protected makeNodeInfo(context: LirContext): LirNodeInfo {
    return { context, ...this.lirInfo }
  }

  /** This reference to the LirRegistry can be used to publish updates */
  protected getLirRegistry() {
    return this.lirInfo.registry
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

export abstract class DefaultLirNode
  extends NodeInfoManager
  implements LirNode
{
  #id: LirId

  constructor(protected readonly nodeInfo: LirNodeInfo) {
    super(nodeInfo)
    this.#id = new LirId()
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
