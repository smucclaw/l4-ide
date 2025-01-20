import type { Properties as CSSProperties } from 'csstype'

/*************************************************
 *********** Types and type combinators **********
 *************************************************/

export type ValueOf<Obj> = Obj[keyof Obj]

/* ====== CSS ============ */

export type CSSClass = string

export type Styles = {
  [val: string]: CSSProperties | Styles
}

/*************************************************
 *************** Other misc utils ****************
 *************************************************/

/* ====== String processing utils ============ */

export function toUpperSnakeCase(input: string): string {
  return input
    .replace(/([a-z])([A-Z])/g, '$1_$2') // Insert underscore between lowercase and uppercase letters
    .toUpperCase()
}
