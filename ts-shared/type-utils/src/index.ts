/**
 * @repo/type-utils - Type-related utils:
 * type classes (Eq, Ord), various useful types, and other type utilities
 * used by layout-ir and visualization libs.
 */

import type { Properties as CSSProperties } from 'csstype'

/*************************************************
 ***************** Branded Types *****************
 *************************************************/

/**
 * Branded types for type-level differentiation
 *
 * Branded types allow you to create distinct types from the same underlying type,
 * preventing accidental mixing of conceptually different values.
 *
 * @see https://egghead.io/blog/using-branded-types-in-typescript
 * See also the TS playground excerpt linked from the article above
 */

declare const __brand: unique symbol
type Brand<B> = { [__brand]: B }

/**
 * Creates a branded type from a base type T with brand B
 *
 * @example
 * type UserId = Branded<string, 'UserId'>
 * type OrderId = Branded<string, 'OrderId'>
 *
 */
export type Branded<T, B> = T & Brand<B>

/*************************************************
 ***** Pseudo Type Classes for Comparison *******
 *************************************************/

/**
 * Type class for equality comparison */
export interface Eq<A> {
  isEqualTo<B extends A>(other: B): boolean
}

/**
 * Result of a comparison operation
 */
export enum ComparisonResult {
  LessThan = -1,
  Equal = 0,
  GreaterThan = 1,
}

/**
 * Type class for ordering comparison */
export interface Ord<T> extends Eq<T> {
  compare(other: T): ComparisonResult
}

/**
 * Utility function to check if first value is less than or equal to second
 *
 * @param a First value to compare
 * @param b Second value to compare
 * @returns true if a <= b, false otherwise
 */
export function isLessThanOrEquals<A extends Ord<A>>(a: A, b: A): boolean {
  return [ComparisonResult.Equal, ComparisonResult.LessThan].includes(
    a.compare(b)
  )
}

/*************************************************
 ***********      HasId     ****************
 *************************************************/

export interface HasId {
  getId(): string
}

/*************************************************
 ******************* CSS Styles *****************
 *************************************************/

export type Styles = {
  [val: string]: CSSProperties | Styles
}
