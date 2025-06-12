import type { LadderEnv } from '$lib/ladder-env'
import type { LirNodeInfo } from '@repo/layout-ir'

export interface LadderLirSource<A, B> {
  toLir(nodeInfo: LirNodeInfo, env: LadderEnv, data: A): Promise<B>
}
