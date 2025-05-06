/********************************************************************
                Ladder Env
---------------------------------------------------------------------
Stuff for Ladder operations
that can be used by components that are children of LadderEnvProvider
*********************************************************************/

import { L4Connection } from './l4-connection.js'
import type { LadderBackendApi } from 'jl4-client-rpc'
import type { LirContext, LirRegistry, LirRootType } from './layout-ir/core.js'
import type { FunDeclLirNode } from './layout-ir/ladder-graph/ladder.svelte.js'
import { setContext, getContext } from 'svelte'
import type { VersionedDocId } from '@repo/viz-expr'

export class LadderEnv {
  /** Initialize the Ladder Env -- which includes setting the fun decl lir node in the Lir Registry */
  static make(
    lirRegistry: LirRegistry,
    versionedDocId: VersionedDocId,
    backendApi: LadderBackendApi
  ) {
    const l4Connection = new L4Connection(backendApi)
    return new LadderEnv(lirRegistry, l4Connection, versionedDocId)
  }

  private constructor(
    private readonly lirRegistry: LirRegistry,
    private readonly l4Connection: L4Connection,
    private readonly versionedDocId: VersionedDocId
  ) {}

  getLirRegistry(): LirRegistry {
    return this.lirRegistry
  }

  getL4Connection(): L4Connection {
    return this.l4Connection
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

  getVersionedTextDocIdentifier(): VersionedDocId {
    return this.versionedDocId
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

/** Lir registry key for the topmost FunDecl Lir Node */
export const LADDER_VIZ_ROOT_TYPE: LirRootType = 'VizFunDecl'
