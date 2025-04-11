import type { Ord } from '$lib/utils.js'
import { ComparisonResult, isLessThanOrEquals } from '$lib/utils.js'

/********************************
      Edge types
*********************************/

export function stringifyEdge<A extends Ord<A>, T extends Edge<A>>(
  edge: T
): string {
  return `<${edge.getU()}, ${edge.getV()}>`
}

/** The most minimal 'Edge' */
export interface Edge<A extends Ord<A>> {
  getU(): A
  getV(): A
}

export interface EdgeWithOrd<A extends Ord<A>>
  extends Edge<A>,
    Ord<EdgeWithOrd<A>> {
  isEqualTo<B extends EdgeWithOrd<A>>(that: B): boolean
  compare(that: this): ComparisonResult
}

export abstract class AbsEdgeWithOrd<A extends Ord<A>>
  implements EdgeWithOrd<A>
{
  readonly u: A
  readonly v: A
  constructor(u: A, v: A) {
    this.u = u
    this.v = v
  }

  getU(): A {
    return this.u
  }

  getV(): A {
    return this.v
  }

  abstract isEqualTo<B extends EdgeWithOrd<A>>(that: B): boolean

  compare(that: this) {
    // lexicographical comparison
    const uComparison = this.getU().compare(that.getU())
    if (uComparison !== ComparisonResult.Equal) return uComparison

    return this.getV().compare(that.getV())
  }
}

export class UndirectedEdge<A extends Ord<A>> extends AbsEdgeWithOrd<A> {
  readonly u: A
  readonly v: A
  constructor(u: A, v: A) {
    super(u, v)
    // Sort input vertices so that this.u <= this.v
    // The same `compare` implementation can then be used for both directed and undirected edges
    if (isLessThanOrEquals(u, v)) {
      this.u = u
      this.v = v
    } else {
      this.v = u
      this.u = v
    }
  }

  override isEqualTo<B extends EdgeWithOrd<A>>(that: B): boolean {
    return (
      that instanceof UndirectedEdge &&
      ((this.u.isEqualTo(that.getU()) && this.v.isEqualTo(that.getV())) ||
        (this.u.isEqualTo(that.getV()) && this.v.isEqualTo(that.getU())))
    )
  }

  toString(): string {
    return `(${this.u}, ${this.v})`
  }
}

/** Note that the string representation of a DirectedEdge differs from that of an UndirectedEdge. */
export class DirectedEdge<A extends Ord<A>> extends AbsEdgeWithOrd<A> {
  constructor(u: A, v: A) {
    super(u, v)
  }

  override isEqualTo<B extends EdgeWithOrd<A>>(that: B): boolean {
    return (
      that instanceof DirectedEdge &&
      this.getU().isEqualTo(that.getU()) &&
      this.getV().isEqualTo(that.getV())
    )
  }

  toString(): string {
    return `<${this.getU()}, ${this.getV()}>`
  }
}
