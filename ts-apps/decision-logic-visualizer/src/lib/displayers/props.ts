import type { RootDisplayerProps, DisplayerProps } from '$lib/layout-ir/core.js'
import type {
  DeclLirNode,
  ValidPathsListLirNode,
} from '$lib/layout-ir/ladder-lir.svelte.js'

/**********************************************************
            Displayer Props
      (for things that are NOT specific to SvelteFlow)
***********************************************************/

export interface LadderFlowDisplayerProps extends RootDisplayerProps {
  node: DeclLirNode
}

export interface BaseLadderFlowDisplayerProps extends DisplayerProps {
  node: LadderFlowDisplayerProps['node']
}

export interface PathListDisplayerProps extends DisplayerProps {
  node: ValidPathsListLirNode
}
