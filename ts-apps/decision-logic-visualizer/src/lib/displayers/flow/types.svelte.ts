import * as SF from '@xyflow/svelte'
import type { LirId, LirContext } from '$lib/layout-ir/core.js'
import type { ExprLirNode } from '$lib/layout-ir/lir-decision-logic.svelte.js'
import type { Ord } from '$lib/utils.js'
import { ComparisonResult } from '$lib/utils.js'
// import { over } from 'lodash'

const DEFAULT_INITIAL_POSITION = { x: 0, y: 0 }

export interface ExprFlowDisplayerProps {
  context: LirContext
  node: ExprLirNode
}

export interface FlowGraph {
  nodes: FlowNode[]
  edges: FlowEdge[]
}

/************************************************
            Flow Nodes
*************************************************/

/** A simplified version of the SvelteFlow Node interface,
 * for the v0 prototype. */
export interface FlowNode extends Ord<FlowNode> {
  getId(): string
  getOrigLirIds(): LirId[]
  toSFPojo(): SF.Node
}

export abstract class BaseFlowNode implements Ord<FlowNode> {
  private static counter = 0
  protected id: string

  constructor(
    /** The LirIds of the LirNodes that correspond in some sense to this FlowNode */
    protected readonly origLirIds: LirId[],
    protected readonly position: {
      x: number
      y: number
    } = DEFAULT_INITIAL_POSITION
  ) {
    this.id = (BaseFlowNode.counter++).toString()
  }

  getId() {
    return this.id
  }

  /** Required for SF nodes */
  getPosition() {
    return this.position
  }

  getOrigLirIds() {
    return this.origLirIds
  }

  abstract toSFPojo(): SF.Node

  isEqualTo<T extends FlowNode>(other: T) {
    return this.getId() === other.getId()
  }

  /** Lexicographical comparison based on IDs.
   * This gets used by DirectedEdge. */
  compare(that: this) {
    const intId = parseInt(this.getId())
    const thatId = parseInt(that.getId())
    if (intId === thatId) {
      return ComparisonResult.Equal
    } else if (intId < thatId) {
      return ComparisonResult.LessThan
    } else {
      return ComparisonResult.GreaterThan
    }
  }
}

export class BoolVarFlowNode extends BaseFlowNode implements Ord<FlowNode> {
  // TODO: Let's only add more info to data, e.g. a value, in the next prototype
  constructor(
    protected readonly data: { label: string },
    origLirIds: LirId[],
    position: {
      x: number
      y: number
    } = DEFAULT_INITIAL_POSITION
  ) {
    super(origLirIds, position)
  }

  getData() {
    return this.data
  }

  toSFPojo(): SF.Node {
    return {
      id: this.id,
      position: this.position,
      data: this.data,
    }
  }
}

/** A FlowNode that's used solely to visually group or 'bundle' other nodes. */
export type GroupingFlowNode = SinkFlowNode | SourceFlowNode

export class SourceFlowNode extends BaseFlowNode implements Ord<FlowNode> {
  constructor(
    origLirIds: LirId[],
    position: {
      x: number
      y: number
    } = DEFAULT_INITIAL_POSITION
  ) {
    super(origLirIds, position)
  }

  toSFPojo(): SF.Node {
    return {
      id: this.id,
      position: this.position,
      data: { label: 'GroupSource' },
    }
  }
}

export class SinkFlowNode extends BaseFlowNode implements Ord<FlowNode> {
  constructor(
    origLirIds: LirId[],
    position: {
      x: number
      y: number
    } = DEFAULT_INITIAL_POSITION
  ) {
    super(origLirIds, position)
  }

  toSFPojo(): SF.Node {
    return {
      id: this.id,
      position: this.position,
      data: { label: 'GroupSink' },
    }
  }
}
/************************************************
            Flow Edges
*************************************************/

export class FlowEdge implements Ord<FlowEdge> {
  id: string
  constructor(
    readonly source: string,
    readonly target: string
  ) {
    this.id = `(${source}, ${target})`
  }

  getU() {
    return this.source
  }

  getV() {
    return this.target
  }

  isEqualTo<T extends FlowEdge>(other: T) {
    return this.id === other.id
  }

  /** Lexicographical comparison of IDs of nodes */
  compare(that: FlowEdge): ComparisonResult {
    if (this.getU() < that.getU()) {
      return ComparisonResult.LessThan
    } else if (this.getU() > that.getU()) {
      return ComparisonResult.GreaterThan
    }

    if (this.getV() < that.getV()) {
      return ComparisonResult.LessThan
    } else if (this.getV() > that.getV()) {
      return ComparisonResult.GreaterThan
    }

    return ComparisonResult.Equal
  }

  toSFPojo(): SF.Edge {
    return {
      id: this.id,
      source: this.source,
      target: this.target,
    }
  }
}
