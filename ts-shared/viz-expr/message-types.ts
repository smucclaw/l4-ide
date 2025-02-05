import type {
  NotificationType,
  // RequestType
} from 'vscode-messenger-common'

import { VisualizeDecisionLogicIRInfo } from './viz-expr.js'

/**
 * See also Wrapper / Protocol interfaces in viz-expr.ts
 */
export const VisualizeDecisionLogicNotification: NotificationType<VisualizeDecisionLogicIRInfo> =
  {
    method: 'visualizeDecisionLogic',
  }
