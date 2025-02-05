import { Schema } from 'effect'
import type {
  NotificationType,
  // RequestType
} from 'vscode-messenger-common'

import { VisualizeDecisionLogicIRInfo } from '@repo/decision-logic-visualizer'

/** The payload for VisualizeDecisionLogicNotification */
export type VisualizeDecisionLogicIRInfo = Schema.Schema.Type<
  typeof VisualizeDecisionLogicIRInfo
>

export const VisualizeDecisionLogicNotification: NotificationType<VisualizeDecisionLogicIRInfo> =
  {
    method: 'visualizeDecisionLogic',
  }
