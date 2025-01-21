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
