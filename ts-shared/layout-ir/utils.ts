import type { Properties as CSSProperties } from 'csstype'

/*************************************************
 *********** Types and type combinators **********
 *************************************************/

/*************************
      Branded types
**************************/

/** https://egghead.io/blog/using-branded-types-in-typescript
 *  See also the TS playground excerpt linked from the article above
 */
declare const __brand: unique symbol
type Brand<B> = { [__brand]: B }
export type Branded<T, B> = T & Brand<B>

/****************************
     Pseudo Eq, Ord
 *****************************/

export interface Eq<A> {
  isEqualTo<B extends A>(other: B): boolean
}

export enum ComparisonResult {
  LessThan = -1,
  Equal = 0,
  GreaterThan = 1,
}

export interface Ord<T> extends Eq<T> {
  compare(other: T): ComparisonResult
}

export function isLessThanOrEquals<A extends Ord<A>>(a: A, b: A): boolean {
  return [ComparisonResult.Equal, ComparisonResult.LessThan].includes(
    a.compare(b)
  )
}

/****************************
          HasId
*****************************/

export interface HasId {
  getId(): string
}

/* ====== CSS ============ */

export type Styles = {
  [val: string]: CSSProperties | Styles
}
