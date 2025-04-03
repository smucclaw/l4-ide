import type { Unique } from '@repo/viz-expr'
import type { Value } from './type.js'
import type { LirId } from '../layout-ir/core.js'

export class Environment {
  #env: Subst

  constructor(initial: Array<Value>) {
    this.#env = new Subst(initial)
  }

  get(unique: Unique): Value | undefined {
    return this.#env.get(unique)
  }

  set(unique: Unique, value: Value) {
    this.#env.set(unique, value)
  }

  getUniques(): Array<Unique> {
    return this.#env.getUniques()
  }

  getEntries(): Array<[Unique, Value]> {
    return this.#env.getEntries()
  }

  getFresh(): Unique {
    return this.#env.getMaxUnique() + 1
  }

  clone() {
    return new Environment(this.#env.asArray())
  }

  getClonedEnvExtendedWith(other: Array<[Unique, Value]>) {
    const newEnv = this.clone()
    for (const [uniq, value] of other) {
      newEnv.set(uniq, value)
    }
    return newEnv
  }
}

/** A mapping from Unique to Value */
export class Subst {
  #subst: Array<Value>

  static fromEntries(entries: Array<[Unique, Value]>) {
    const initial: Array<Value> = []
    entries.forEach(([unique, value]) => {
      initial[unique] = value
    })
    return new Subst(initial)
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

  getMaxUnique(): Unique {
    return this.#subst.length - 1
  }

  getUniques(): Array<Unique> {
    return this.#subst.map((_, i) => i)
  }

  getEntries(): Array<[Unique, Value]> {
    return this.#subst.map((value, i) => [i, value])
  }

  asArray(): Array<Value> {
    return this.#subst.slice()
  }

  clone() {
    return new Subst(this.#subst.slice())
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
