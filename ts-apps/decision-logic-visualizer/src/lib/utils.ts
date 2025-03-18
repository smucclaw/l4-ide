import type { Properties as CSSProperties } from 'csstype'

/*************************************************
 *********** Types and type combinators **********
 *************************************************/

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

/*****************************
      Shadcn-Svelte Utils
******************************/

import type { ClassValue } from 'clsx'
import { clsx } from 'clsx'
import { twMerge } from 'tailwind-merge'

export function cn(...inputs: ClassValue[]) {
  return twMerge(clsx(inputs))
}
