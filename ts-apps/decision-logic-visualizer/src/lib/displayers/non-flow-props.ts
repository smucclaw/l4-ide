import type { DisplayerProps } from '$lib/layout-ir/core.js'
import type { PathsListLirNode } from '$lib/layout-ir/paths-list.js'

export interface PathListDisplayerProps extends DisplayerProps {
  node: PathsListLirNode
}
