import type { DisplayerProps } from '$lib/layout-ir/core.js'
import type { ValidPathsListLirNode } from '$lib/layout-ir/paths-list.js'

export interface PathListDisplayerProps extends DisplayerProps {
  node: ValidPathsListLirNode
}
