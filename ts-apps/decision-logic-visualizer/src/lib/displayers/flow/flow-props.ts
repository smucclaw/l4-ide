import type { RootDisplayerProps, DisplayerProps } from '$lib/layout-ir/core.js'
import type { FunDeclLirNode } from '$lib/layout-ir/ladder-graph/ladder.svelte.js'
import type { LadderBackendApi } from 'jl4-client-rpc'
import { VersionedDocId } from '@repo/viz-expr'

export interface LadderFlowDisplayerProps extends RootDisplayerProps {
  node: FunDeclLirNode
  backendApi: LadderBackendApi
  versionedDocId: VersionedDocId
}

/** For flow-base.svelte */
export interface BaseLadderFlowDisplayerProps extends DisplayerProps {
  node: LadderFlowDisplayerProps['node']
}
