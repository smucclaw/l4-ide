/*
If you're unfamiliar with Lir stuff,
start by looking at the docs for LirNode.

I had originally adapted the Lir stuff from a similar framework
that I learned from Jimmy Koppel (https://www.jameskoppel.com/),
but this framework is now getting quite different from what it used to be.
*/

import { ComparisonResult } from '../utils.js'
import type { DirectedAcyclicGraph } from '../algebraic-graphs/dag.js'
import { Empty } from '../algebraic-graphs/dag.js'
import type { LadderLirNode } from './lir-decision-logic.svelte.js'

/*********************************************
       Registry
***********************************************/
export type LirRootType = string

/** Lir := 'Layout IR' */
export class LirRegistry {
  #roots: Map<LirRootType, LirNode> = new Map()
  // Will add subscribers here in the future

  // @typescript-eslint/no-unused-vars
  getRoot(_context: LirContext, rootType: LirRootType): LirNode | undefined {
    return this.#roots.get(rootType)
  }

  // @typescript-eslint/no-unused-vars
  setRoot(_context: LirContext, rootType: LirRootType, node: LirNode) {
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

  compare(other: LirId) {
    const thisStr = this.toString()
    const otherStr = other.toString()

    if (thisStr < otherStr) return ComparisonResult.LessThan
    if (thisStr > otherStr) return ComparisonResult.GreaterThan
    return ComparisonResult.Equal
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
    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    const { context, ...lirInfoWithoutContext } = defaultNodeInfo
    this.lirInfo = lirInfoWithoutContext
  }

  protected makeNodeInfo(context: LirContext): LirNodeInfo {
    return { context, ...this.lirInfo }
  }

  /** This reference to the LirRegistry can be used to publish updates */
  protected getRegistry() {
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

  // getChildren(context: LirContext): LirNode[]

  /** NON-pretty */
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

  compare(other: LirNode) {
    return this.getId().compare(other.getId())
  }

  // Removed getChildren to simplify things

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
 *
 * This LirContext has been specialized to the LirFlow setting
 */
export class LirContext {
  /** Can contain both FlowLirNodes and non-FlowLirNodes */
  #nodes: Map<LirId, LirNode> = new Map()

  // TODO: Not 100% sure if the DAG shld be here or in LirRegistry,
  // but won't be too hard to move later if necessary
  /** For the flow lir graph */
  #dag: DirectedAcyclicGraph<LirId>

  constructor() {
    this.#dag = new Empty()
  }

  get(id: LirId) {
    return this.#nodes.get(id)
  }

  set(node: LirNode) {
    this.#nodes.set(node.getId(), node)
  }

  /** Specifically for LadderLirNode. I.e., the `id` should correspond to that of a LadderLirNode. */
  getNeighbors(id: LirId): LadderLirNode[] {
    const neighbors = this.#dag.getAdjMap().get(id) || new Set()

    return Array.from(neighbors)
      .map((neighborId) => this.get(neighborId) as LadderLirNode)
      .filter((n) => !!n)
  }

  // TODO
  // getEdge(u: LirId, v: LirId): LirEdge | undefined {
  //   const uNode = this.#dag.getAdjMap().get(u)
  //   const vNode = this.#dag.getAdjMap().get(v)
  //   if (!uNode || !vNode) return undefined

  //   // TODO
  //   // Make a LirEdge that also has any data associated with the edge

  // }

  getAllPaths(): DirectedAcyclicGraph<LirId>[] {
    return this.#dag.getAllPaths()
  }

  /** My first-pass, naive approach is to make the Dag separately from the LirContext, and only set the Dag in the LirContext after the LirNodes and Dag have been made.
  This obviously is not ideal
  (e.g., it opens up the possibility that the Dag that's set might be out of sync with the LirNodes) --- it'd
  be much better to somehow construct them in tandem --- but
  it's probably OK as a first version. */
  setDag(dag: DirectedAcyclicGraph<LirId>) {
    this.#dag = dag
  }
}

/*********************************************
       Lir Data Source
***********************************************/

export interface LirSource<A, B> {
  toLir(nodeInfo: LirNodeInfo, data: A): B
}
