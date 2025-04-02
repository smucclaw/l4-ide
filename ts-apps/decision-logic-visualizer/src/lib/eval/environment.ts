import type { Unique } from '@repo/viz-expr'
import type { Value } from './type.js'
import type { LirId } from '../layout-ir/core.js'

/** A mapping from Unique to Value */
export class Subst {
  #subst: Array<Value>

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
}

/**
 * Coreferents: mapping from Unique to Set<LirId>
 */
export class Corefs {
  // A Unique is a non-negative integer
  #coreferents: Array<Set<LirId>>

  constructor(initialCoreferents: Array<Set<LirId>>) {
    this.#coreferents = initialCoreferents
  }

  getCoreferents(unique: Unique) {
    return this.#coreferents[unique] ?? new Set<LirId>()
  }

  dispose() {
    this.#coreferents = []
  }
}
