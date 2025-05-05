/*
If you're unfamiliar with Lir stuff,
start by looking at the docs for LirNode.

I had originally adapted the Lir stuff from a similar framework
that I learned from Jimmy Koppel (https://www.jameskoppel.com/),
but this framework is now getting quite different from what it used to be.
*/

import { ComparisonResult } from '../utils.js'

/*********************************************
       Registry, Top-level Lir
***********************************************/

export type Unsubscriber = { unsubscribe: () => void }

export type LirRootType = string

/** Lir := 'Layout IR' */
export class LirRegistry {
  #roots: Map<LirRootType, LirNode> = new Map()
  #subscribers: Map<symbol, (context: LirContext, id: LirId) => void> =
    new Map()

  constructor() {}

  // @typescript-eslint/no-unused-vars
  getRoot(_context: LirContext, rootType: LirRootType): LirNode | undefined {
    return this.#roots.get(rootType)
  }

  // @typescript-eslint/no-unused-vars
  setRoot(_context: LirContext, rootType: LirRootType, node: LirNode) {
    this.#roots.set(rootType, node)
  }

  subscribe(callback: (_: LirContext, id: LirId) => void): Unsubscriber {
    const callbackId = Symbol()
    this.#subscribers.set(callbackId, callback)
    return {
      unsubscribe: () => this.#subscribers.delete(callbackId),
    }
  }

  publish(context: LirContext, id: LirId) {
    // must NOT use forEach on pain of running into issues with Safari, at least for me
    for (const callback of this.#subscribers.values()) {
      callback(context, id)
    }
  }
}

export class LirId {
  private static counter = 0

  constructor(private id: symbol = Symbol(LirId.counter++)) {}

  toString() {
    return this.id.description as string
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

  // Removed getChildren for now to simplify things

  /** NON-pretty */
  toString(): string

  dispose(context: LirContext): void
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

  abstract toString(): string

  abstract dispose(context: LirContext): void
}

/*********************************************
       Viz Config
***********************************************/

export interface VizConfig {
  constants: {
    readonly EXPLANATORY_AND_EDGE_LABEL: string
    readonly OR_BUNDLING_NODE_LABEL: string
  }
  shouldEnableZenMode: boolean
}

const defaultVizConfig: VizConfig = {
  constants: {
    EXPLANATORY_AND_EDGE_LABEL: 'AND',
    /** aka anyOfBundlingNodeAnno.annotation */
    OR_BUNDLING_NODE_LABEL: 'ANY OF',
  },
  shouldEnableZenMode: false,
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

  constructor(private config: VizConfig = defaultVizConfig) {}

  get(id: LirId) {
    return this.#nodes.get(id)
  }

  set(node: LirNode) {
    this.#nodes.set(node.getId(), node)
  }

  clear(id: LirId) {
    this.#nodes.delete(id)
  }

  /***************
     Viz Config 
  ****************/

  // Zen mode

  shouldEnableZenMode() {
    return this.config.shouldEnableZenMode
  }

  enableZenMode() {
    this.config.shouldEnableZenMode = true
  }

  disableZenMode() {
    this.config.shouldEnableZenMode = false
  }

  // Constants
  // (The rough thought for now is, if we want to change these,
  // we'd make and pass in a different Context. But not sure.)

  getExplanatoryAndEdgeLabel() {
    return this.config.constants.EXPLANATORY_AND_EDGE_LABEL
  }

  getOrBundlingNodeLabel() {
    return this.config.constants.OR_BUNDLING_NODE_LABEL
  }
}

/*************************************************
 ****************** Displayers *******************
 *************************************************/

export interface DisplayerProps {
  context: LirContext
  node: LirNode
}

export interface RootDisplayerProps extends DisplayerProps {
  /** The root displayer will set the `lir` in the Svelte context so that children displayers can also access it */
  lir: LirRegistry
}
