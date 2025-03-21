import type { Ord } from '$lib/utils.js'
import { ComparisonResult } from '$lib/utils.js'
import { AbsEdgeWithOrd } from './edge.js'

/**************************************************************
  Base internal Adjacency Map implementation / representation
***************************************************************/

export abstract class BaseAMGraph<A extends Ord<A>> {
  protected adjacencyMap: Map<A, Set<A>>

  constructor(adjacencyMap?: Map<A, Set<A>>) {
    this.adjacencyMap = adjacencyMap ?? new Map()
  }

  // Getters and setters for the underlying adjacency map
  getAdjMap() {
    return this.adjacencyMap
  }

  protected setAdjMap(map: typeof this.adjacencyMap) {
    this.adjacencyMap = map
  }

  // Misc useful

  /** Check if a graph contains a given vertex. */
  hasVertex(vertex: A): boolean {
    return this.adjacencyMap.has(vertex)
  }

  /** Check if a graph contains a given edge. */
  hasEdge(u: A, v: A): boolean {
    const neighbors = this.adjacencyMap.get(u)
    if (neighbors) {
      return neighbors.has(v)
    }
    return false
  }

  /** Get a sorted (via `compare`) array of vertices */
  getVertices(): A[] {
    return Array.from(this.adjacencyMap.keys()).sort((a, b) => a.compare(b))
  }

  abstract getEdges(): AbsEdgeWithOrd<A>[]

  /** Internal helper: Get a sorted array of *all* the [A, A] edges */
  protected getAllEdges(): Array<[A, A]> {
    const vertices = this.getVertices()

    // Get [ <v, n> for each vertex v, for each neighbor n of v]
    const allEdges = vertices.flatMap((vertex) => {
      const neighbors = this.adjacencyMap.get(vertex) ?? new Set<A>()
      return Array.from(neighbors).map(
        (neighbor) => [vertex, neighbor] as [A, A]
      )
    })

    return allEdges.toSorted(([u1, v1], [u2, v2]) => {
      const firstComparison = u1.compare(u2)
      return firstComparison !== ComparisonResult.Equal
        ? firstComparison
        : v1.compare(v2)
    })
  }

  /** Pretty prints the *un*directed edges for undirected graphs (and directed for directed graphs)  */
  pPrint(): string {
    const vertices = this.getVertices()
    const edges = this.getEdges().map((edge) => edge.toString())

    if (vertices.length === 0) return 'empty'
    if (edges.length === 0) return `vertices [${vertices.join(', ')}]`
    return `edges [${edges.join(', ')}]`
  }

  /** Stringifies the internal representation. Currently for internal use. */
  toString(): string {
    const vertices = this.getVertices()
    const stringifiedVertices = `vertices [${vertices.join(', ')}]`
    const edges = this.getAllEdges().map((edge) => `<${edge.toString()}>`)

    if (vertices.length === 0) return 'empty'
    if (edges.length === 0) return stringifiedVertices
    return `${stringifiedVertices}\n edges [${edges.join(', ')}]`
  }

  dispose() {
    this.adjacencyMap.clear()
  }
}
