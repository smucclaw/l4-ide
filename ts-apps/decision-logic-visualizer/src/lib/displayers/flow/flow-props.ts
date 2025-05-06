import type { DisplayerProps } from '$lib/layout-ir/core.js'
import type { FunDeclLirNode } from '$lib/layout-ir/ladder-graph/ladder.svelte.js'
import type { LadderEnv } from '$lib/ladder-env.js'

export interface RootDisplayerProps extends DisplayerProps {
  env: LadderEnv
}

export interface LadderFlowDisplayerProps extends RootDisplayerProps {
  node: FunDeclLirNode
}

/** For flow-base.svelte */
export interface BaseLadderFlowDisplayerProps extends DisplayerProps {
  node: LadderFlowDisplayerProps['node']
}
