import type { Properties as CSSProperties } from 'csstype'

/*************************************************
 *********** Types and type combinators **********
 *************************************************/

/****************************
     Pseudo Eq, Ord
 *****************************/

export interface Eq<T> {
  isEqualTo(other: T): boolean
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

export type ValueOf<Obj> = Obj[keyof Obj]

/* ====== CSS ============ */

export type CSSClass = string

export type Styles = {
  [val: string]: CSSProperties | Styles
}
