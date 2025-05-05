import type { LadderEnv } from '$lib/ladder-env'
import type { LirNodeInfo } from './core.js'

export interface LadderLirSource<A, B> {
  toLir(nodeInfo: LirNodeInfo, env: LadderEnv, data: A): B
}
