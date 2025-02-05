import type { IRExpr } from '@repo/viz-expr'
/*
Do not use $lib for the layout-ir imports
*/
import type { LirSource, LirNodeInfo } from '../layout-ir/core.js'
import type { ExprLirNode } from '../layout-ir/lir-decision-logic.svelte.js'
import {
  BoolVarLirNode,
  // NotLirNode,
  BinExprLirNode,
} from '../layout-ir/lir-decision-logic.svelte.js'
import { match } from 'ts-pattern'

/***********************************
        Lir Data Sources
************************************/

export const ExprLirSource: LirSource<IRExpr, ExprLirNode> = {
  toLir(nodeInfo: LirNodeInfo, expr: IRExpr): ExprLirNode {
    return (
      match(expr)
        .with({ $type: 'BoolVar' }, (ap) => new BoolVarLirNode(nodeInfo, ap))
        // .with(
        //   { $type: 'Not' },
        //   (n) => new NotLirNode(nodeInfo, ExprSource.toLir(nodeInfo, n.negand))
        // )
        .with(
          { $type: 'BinExpr' },
          (binE) =>
            new BinExprLirNode(
              nodeInfo,
              binE.op,
              ExprLirSource.toLir(nodeInfo, binE.left),
              ExprLirSource.toLir(nodeInfo, binE.right)
            )
        )
        .exhaustive()
    )
  },
}
