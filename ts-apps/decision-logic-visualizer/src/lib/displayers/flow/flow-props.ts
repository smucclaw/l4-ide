import type { RootDisplayerProps, DisplayerProps } from '$lib/layout-ir/core.js'
import type { FunDeclLirNode } from '$lib/layout-ir/ladder-graph/ladder.svelte.js'
import type { LadderBackendApi } from 'jl4-client-rpc'

export interface LadderFlowDisplayerProps extends RootDisplayerProps {
  node: FunDeclLirNode
  backendApi: LadderBackendApi
}

/** For flow-base.svelte */
export interface BaseLadderFlowDisplayerProps extends DisplayerProps {
  node: LadderFlowDisplayerProps['node']
}
