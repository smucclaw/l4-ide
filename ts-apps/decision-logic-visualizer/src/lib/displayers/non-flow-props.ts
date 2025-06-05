import type { DisplayerProps } from '$lib/layout-ir/core.js'
import type { PathsListLirNode } from '$lib/layout-ir/paths-list.js'
import type { NNFLadderGraphLirNode } from '$lib/layout-ir/ladder-graph/ladder.svelte.js'

export interface PathListDisplayerProps extends DisplayerProps {
  node: PathsListLirNode
  ladderGraph: NNFLadderGraphLirNode
}
