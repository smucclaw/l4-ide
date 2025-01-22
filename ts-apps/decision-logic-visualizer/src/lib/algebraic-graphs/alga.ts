import type { AMUndirectedGraph } from './adjacency-map'

/****************************
  Alga abstract interface
*****************************/

/** Algebraic graph */
export type UndirectedGraph<A extends Ord> = AMUndirectedGraph<A>

/****************************
    Pseudo Eq, Ord
*****************************/
// TODO: This is useful outside of alga too, so move it out later

/** For the underlying vertices */
export interface Eq {
  isEqualTo(other: unknown): boolean
}

export enum ComparisonResult {
  LessThan = -1,
  Equal = 0,
  GreaterThan = 1,
}

export interface Ord extends Eq {
  compare(other: unknown): ComparisonResult
}

export function isLessThanOrEquals<A extends Ord>(a: A, b: A): boolean {
  return [ComparisonResult.Equal, ComparisonResult.LessThan].includes(
    a.compare(b)
  )
}
