import type { IRExpr } from '@repo/viz-expr'
/*
Do not use $lib for the layout-ir imports
*/
import type { LirSource, LirNodeInfo } from '../layout-ir/core.js'
import type { ExprLirNode } from '../layout-ir/lir-decision-logic.svelte.js'
import {
  BoolVarLirNode,
  AndLirNode,
  OrLirNode,
  NotLirNode,
} from '../layout-ir/lir-decision-logic.svelte.js'
import { match } from 'ts-pattern'

/***********************************
        Lir Data Sources
************************************/

export const ExprLirSource: LirSource<IRExpr, ExprLirNode> = {
  toLir(nodeInfo: LirNodeInfo, expr: IRExpr): ExprLirNode {
    return match(expr)
      .with(
        { $type: 'BoolVar' },
        (originalVar) => new BoolVarLirNode(nodeInfo, originalVar)
      )
      .with(
        { $type: 'Not' },
        (n) => new NotLirNode(nodeInfo, ExprLirSource.toLir(nodeInfo, n.negand))
      )
      .with(
        { $type: 'And' },
        (andExpr) =>
          new AndLirNode(
            nodeInfo,
            andExpr.args.map((arg) => ExprLirSource.toLir(nodeInfo, arg))
          )
      )
      .with(
        { $type: 'Or' },
        (orExpr) =>
          new OrLirNode(
            nodeInfo,
            orExpr.args.map((arg) => ExprLirSource.toLir(nodeInfo, arg))
          )
      )
      .exhaustive()
  },
}
