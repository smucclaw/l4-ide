import type { Unique } from '@repo/viz-expr'
import type { UBoolVal } from './type.js'

/** A mapping from Unique to UBoolVal */
export class Assignment {
  #subst: Array<UBoolVal>

  static fromEntries(entries: Array<[Unique, UBoolVal]>) {
    const initial: Array<UBoolVal> = []
    entries.forEach(([unique, value]) => {
      initial[unique] = value
    })
    return new Assignment(initial)
  }

  constructor(initial: Array<UBoolVal>) {
    this.#subst = initial
  }

  /** Set binding */
  set(unique: Unique, value: UBoolVal) {
    this.#subst[unique] = value
  }

  /** Get binding */
  get(unique: Unique): UBoolVal | undefined {
    return this.#subst[unique]
  }

  getUniques(): Array<Unique> {
    return this.#subst.map((_, i) => i)
  }

  getEntries(): Array<[Unique, UBoolVal]> {
    // Need the filter because the underlying array can be sparse / have holes
    return Array.from(this.#subst.entries()).filter(
      ([, value]) => value !== undefined
    )
  }

  asArray(): Array<UBoolVal> {
    return this.#subst.slice()
  }

  clone() {
    return new Assignment(this.#subst.slice())
  }
}
