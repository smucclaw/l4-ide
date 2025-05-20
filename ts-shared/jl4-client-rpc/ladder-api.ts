import type { L4RpcRequestType } from './custom-protocol.js'
import type { RenderAsLadderInfo } from '@repo/viz-expr'

/** The interface that the Ladder frontend components
 * (most proximally, the webview, and the visualizer, indirectly)
 * use to talk to the Ladder backend */
export type LadderBackendApi = L4BackendApi

/**
 * Can think of this as being analogous to Lean's [EditorApi](https://github.com/leanprover/vscode-lean4/blob/44bf3e6ce3a151ab457c061e16be2508066a518d/lean4-infoview-api/src/infoviewApi.ts#L34),
 * except that we don't put editor stuff here.
 *
 * This is a minimal set of lower-level primitives
 * upon which we can build higher-level interfaces like {@link L4BackendConnection}.
 */
interface L4BackendApi {
  /** Make a request to the LSP server. */
  sendClientRequest<P extends object, R>(
    type: L4RpcRequestType<P, R>,
    params: P
  ): Promise<R | null>

  /* Send a notification to the LSP server. [TODO] */

  /** Update / re-make the LadderFlow component */
  updateViz(renderLadderInfo: RenderAsLadderInfo): Promise<void>
}

/** A mock implementation of the LadderBackendApi for testing / dev-ing */
export const mockLadderBackendApi: LadderBackendApi = {
  sendClientRequest: async (type, params) => {
    console.log('[mockLadderBackendApi] sendClientRequest: ', type, params)
    return Promise.resolve(null)
  },

  updateViz: async (renderLadderInfo) => {
    console.log('[mockLadderBackendApi] updateViz: ', renderLadderInfo)
  },
}
