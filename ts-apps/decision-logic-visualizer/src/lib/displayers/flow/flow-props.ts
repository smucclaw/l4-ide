import type { RootDisplayerProps, DisplayerProps } from '$lib/layout-ir/core.js'
import type { DeclLirNode } from '$lib/layout-ir/ladder-lir.svelte.js'

export interface LadderFlowDisplayerProps extends RootDisplayerProps {
  node: DeclLirNode
}

/** For flow-base.svelte */
export interface BaseLadderFlowDisplayerProps extends DisplayerProps {
  node: LadderFlowDisplayerProps['node']
}
