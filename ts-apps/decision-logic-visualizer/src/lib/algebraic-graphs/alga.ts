import type { AMGraph } from './adjacency-map'

/****************************
  Alga abstract interface
*****************************/

/** Algebraic graph */
export type Graph<A extends Eq> = AMGraph<A>

/****************************
    A pseudo Eq
*****************************/
// TODO: This is useful outside of alga too, so move it out later

/** For the underlying vertices */
export interface Eq {
  isEqualTo(other: this): boolean
}
