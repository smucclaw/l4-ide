import type { Unique } from '@repo/viz-expr'
import type { Value } from './type.js'
import type { LirId } from '../layout-ir/core.js'

/** A mapping from Unique to Value */
export class Assignment {
  #subst: Array<Value>

  static fromEntries(entries: Array<[Unique, Value]>) {
    const initial: Array<Value> = []
    entries.forEach(([unique, value]) => {
      initial[unique] = value
    })
    return new Assignment(initial)
  }

  constructor(initial: Array<Value>) {
    this.#subst = initial
  }

  /** Set binding */
  set(unique: Unique, value: Value) {
    this.#subst[unique] = value
  }

  /** Get binding */
  get(unique: Unique): Value | undefined {
    return this.#subst[unique]
  }

  getUniques(): Array<Unique> {
    return this.#subst.map((_, i) => i)
  }

  getEntries(): Array<[Unique, Value]> {
    // Need the filter because the underlying array can be sparse / have holes
    return Array.from(this.#subst.entries()).filter(
      ([, value]) => value !== undefined
    )
  }

  asArray(): Array<Value> {
    return this.#subst.slice()
  }

  clone() {
    return new Assignment(this.#subst.slice())
  }
}

/**
 * Coreferents: mapping from Unique to Set<LirId>
 */
export class Corefs {
  // A Unique is a non-negative integer
  #coreferents: Array<Set<LirId>>

  static fromEntries(entries: Array<[Unique, LirId]>): Corefs {
    const initialCoreferents: Array<Set<LirId>> = []
    entries.forEach(([unique, lirId]) => {
      if (!initialCoreferents[unique]) {
        initialCoreferents[unique] = new Set([lirId])
      } else {
        initialCoreferents[unique].add(lirId)
      }
    })
    return new Corefs(initialCoreferents)
  }

  private constructor(initialCoreferents: Array<Set<LirId>>) {
    this.#coreferents = initialCoreferents
  }

  getCoreferents(unique: Unique) {
    return this.#coreferents[unique] ?? new Set<LirId>()
  }

  dispose() {
    this.#coreferents = []
  }
}
