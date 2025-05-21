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
  /** Initialize the Ladder Env */
  static make(
    lirRegistry: LirRegistry,
    versionedDocId: VersionedDocId,
    backendApi: LadderBackendApi,
    LADDER_VIZ_ROOT_TYPE: LirRootType
  ) {
    const l4Connection = new L4Connection(backendApi)
    return new LadderEnv(
      lirRegistry,
      l4Connection,
      versionedDocId,
      LADDER_VIZ_ROOT_TYPE
    )
  }

  private constructor(
    private readonly lirRegistry: LirRegistry,
    private readonly l4Connection: L4Connection,
    private readonly versionedDocId: VersionedDocId,
    private readonly LADDER_VIZ_ROOT_TYPE: LirRootType
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
      this.LADDER_VIZ_ROOT_TYPE
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
