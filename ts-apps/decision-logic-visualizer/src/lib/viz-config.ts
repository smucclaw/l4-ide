/*********************************************
       Viz Config
***********************************************/

export interface VizConfig {
  constants: {
    readonly EXPLANATORY_AND_EDGE_LABEL: string
    readonly OR_BUNDLING_NODE_LABEL: string
  }
  shouldEnableZenMode: boolean
}

export const defaultVizConfig: VizConfig = {
  constants: {
    EXPLANATORY_AND_EDGE_LABEL: 'AND',
    /** aka anyOfBundlingNodeAnno.annotation */
    OR_BUNDLING_NODE_LABEL: 'ANY OF',
  },
  shouldEnableZenMode: false,
}
