/******************************************************************
                Ladder Env
-------------------------------------------------------------------
* Stuff for Ladder operations
  that can be used by components that are children of the component where `initializeLadderEnv` is called.
* Conceptually similar to an Env for a Reader monad in Haskell.
*******************************************************************/

import type { LirContext, LirRegistry, LirRootType } from './layout-ir/core.js'
import {
  getLirRegistryFromSvelteContext,
  setLirRegistryInSvelteContext,
} from './layout-ir/core.js'
import type { FunDeclLirNode } from './layout-ir/ladder-graph/ladder.svelte.js'

/** Internal */
const LADDER_VIZ_ROOT_TYPE: LirRootType = 'VizFunDecl'

/** This is conceptually similar to initializing an Env for a Reader monad,
 * though there isn't an explicit Env type here.
 */
export function initializeLadderEnv(
  context: LirContext,
  lirRegistry: LirRegistry,
  funDeclLirNode: FunDeclLirNode
) {
  /*---- An 'env' of sorts for Lir operations -------*/
  // Make the Lir Registry available to children components
  setLirRegistryInSvelteContext(lirRegistry)

  // Similarly with the funDeclLirNode, which is the overall source / root of our Lir structure.
  lirRegistry.setRoot(context, LADDER_VIZ_ROOT_TYPE, funDeclLirNode)

  // Will add an 'env' for non-Lir stuff in the future (e.g. a logger)
}

export function getLirRegistry(): LirRegistry {
  return getLirRegistryFromSvelteContext()
}

/** Aka: 'get the topmost Decide',
 * or 'get the root / overall source of the Ladder Lir structure'.
 *
 * This assumes that `initializeLadderLirEnv` has already been invoked
 * in a parent component.
 */
export function getTopFunDeclLirNode(context: LirContext): FunDeclLirNode {
  const lirRegistry = getLirRegistry()
  return lirRegistry.getRoot(context, LADDER_VIZ_ROOT_TYPE) as FunDeclLirNode
}
