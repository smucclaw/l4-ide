/* 
If you're unfamiliar with Lir stuff, 
start by looking at the docs for LirNode.

Acknowledgments: I adapted the Lir stuff from a similar framework
that I learned from Jimmy Koppel (https://www.jameskoppel.com/).
*/

/*********************************************
       Registry
***********************************************/
export type LirRootType = string

/** Lir := 'Layout IR' */
export class LirRegistry {
  #roots: Map<LirRootType, LirNode> = new Map()
  // Will add subscribers here in the future

  getRoot(context: LirContext, rootType: LirRootType): LirNode | undefined {
    return this.#roots.get(rootType)
  }

  setRoot(context: LirContext, rootType: LirRootType, node: LirNode) {
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

/**
 * Think of LirNodes as being an intermediate representation that's neither the underlying data nor the concrete UI 'displayers'.
 * It's an IR that's focused on content as opposed to presentation --- it can be rendered in different ways, via different concrete GUIs.
 * It can also be used for data synchronization.
 * And it's intended to be extensible in the set of LirNode types/variants.
 */
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

    nodeInfo.context.set(this)
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

/**
 * There will be one LirContext for the entire application;
 * the LirContext represents relevant global data.
 *
 * Operations on LirNodes should have the LirContext as an opaque context parameter.
 * This makes it easier to add, e.g., various kinds of synchronization in the future.
 */
export class LirContext {
  #nodes: Map<LirId, LirNode> = new Map()

  constructor() {}

  get(id: LirId) {
    return this.#nodes.get(id)
  }

  set(node: LirNode) {
    this.#nodes.set(node.getId(), node)
  }
}

/*********************************************
       Lir Data Source
***********************************************/

export interface LirSource<A, B> {
  toLir(nodeInfo: LirNodeInfo, data: A): B
}
