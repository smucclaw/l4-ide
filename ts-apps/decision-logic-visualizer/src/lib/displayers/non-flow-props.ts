import type { DisplayerProps } from '$lib/layout-ir/core.js'
import type { ValidPathsListLirNode } from '$lib/layout-ir/ladder-lir.svelte.js'

export interface PathListDisplayerProps extends DisplayerProps {
  node: ValidPathsListLirNode
}
