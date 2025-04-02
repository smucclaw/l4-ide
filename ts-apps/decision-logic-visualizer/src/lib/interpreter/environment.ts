import type { Unique } from '@repo/viz-expr'
import type { Value } from './value.js'
import type { LirId } from '../layout-ir/core.js'

export class Environment {
  // A Unique is a non-negative integer
  #coreferents: Array<Set<LirId>>

  /** Strictly speaking, we don't need the #env to do what we currently do */
  #env: Array<Value>

  constructor(initialEnv: Array<Value>, initialCoreferents: Array<Set<LirId>>) {
    this.#env = initialEnv
    this.#coreferents = initialCoreferents
  }

  getCoreferents(unique: Unique) {
    return this.#coreferents[unique] ?? new Set<LirId>()
  }

  /** Set binding */
  set(binding: { unique: Unique; value: Value }) {
    this.#env[binding.unique] = binding.value
  }

  /** Get binding */
  get(unique: Unique): Value | undefined {
    return this.#env[unique]
  }

  dispose() {
    this.#env = []
    this.#coreferents = []
  }
}
