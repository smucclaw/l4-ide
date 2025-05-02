/********************************************************************
                Ladder Env
---------------------------------------------------------------------
Stuff for Ladder operations
that can be used by components that are children of LadderEnvProvider
*********************************************************************/

import type { LirContext, LirRegistry, LirRootType } from './layout-ir/core.js'
import type { FunDeclLirNode } from './layout-ir/ladder-graph/ladder.svelte.js'
import { setContext, getContext } from 'svelte'

export class LadderEnv {
  /** Initialize the Ladder Env -- which includes setting the fun decl lir node in the Lir Registry */
  static make(
    context: LirContext,
    lirRegistry: LirRegistry,
    funDeclLirNode: FunDeclLirNode
  ) {
    // Set the top fun decl lir node in Lir Registry
    lirRegistry.setRoot(context, LADDER_VIZ_ROOT_TYPE, funDeclLirNode)

    return new LadderEnv(lirRegistry)
  }

  private constructor(private readonly lirRegistry: LirRegistry) {}

  getLirRegistry(): LirRegistry {
    return this.lirRegistry
  }

  /** Aka: 'get the topmost Decide',
   * or 'get the root / overall source of the Ladder Lir structure'.
   */
  getTopFunDeclLirNode(context: LirContext): FunDeclLirNode {
    return this.lirRegistry.getRoot(
      context,
      LADDER_VIZ_ROOT_TYPE
    ) as FunDeclLirNode
  }

  setInSvelteContext() {
    setContext(ladderEnvKeyForSvelteContext, this)
  }
}

/*********************************
  Ladder Env in Svelte Context
**********************************/

/** Internal */
const ladderEnvKeyForSvelteContext = 'ladderEnv'

export function useLadderEnv(): LadderEnv {
  return getContext(ladderEnvKeyForSvelteContext) as LadderEnv
}

/*********************************
      Ladder Viz Root Type
**********************************/

/** Internal */
const LADDER_VIZ_ROOT_TYPE: LirRootType = 'VizFunDecl'
