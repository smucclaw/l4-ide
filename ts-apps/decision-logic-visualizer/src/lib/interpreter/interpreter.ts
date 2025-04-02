import { Environment } from './environment'
import {
  type Value,
  type BoolVal,
  TrueVal,
  FalseVal,
  UnknownVal,
  ResultValue,
} from './value.js'
import { LirContext } from '../layout-ir/core.js'
import type { VarLirNode } from '../layout-ir/ladder-lir.svelte.js'
import type { BoolVar, Unique, Name } from '@repo/viz-expr'

export class Interpreter {
  #environment: Environment

  constructor() {
    this.#environment = new Environment()
  }

  /*****************************
          Bindings
  ******************************/

  updateEnvWithBinding(
    context: LirContext,
    binding: { unique: Unique; value: Value }
  ) {
    // Set the new binding
    this.#environment.set(binding)

    // Update all VarLirNodes with this Unique with the new Value
    const corefs = Array.from(this.#environment.getCoreferents(binding.unique))
    // console.log('corefs: ', corefs)
    corefs.forEach((coref) => {
      const node = context.get(coref) as VarLirNode
      node._setValue(context, binding.value)
    })
  }

  interp(): ResultValue {}
}
