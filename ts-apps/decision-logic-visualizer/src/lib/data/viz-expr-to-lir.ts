import type { IRDecl, IRExpr, IRId } from '@repo/viz-expr'
/*
Do not use $lib for the layout-ir imports
*/
import type { LirSource, LirId, LirNodeInfo } from '../layout-ir/core.js'
import type { DeclLirNode } from '../layout-ir/ladder-lir.svelte.js'
import {
  FunDeclLirNode,
  BoolVarLirNode,
  NotStartLirNode,
  NotEndLirNode,
  SourceNoAnnoLirNode,
  SourceWithOrAnnoLirNode,
  SinkLirNode,
  LadderGraphLirNode,
  augmentEdgesWithExplanatoryLabel,
} from '../layout-ir/ladder-lir.svelte.js'
import type { DirectedAcyclicGraph } from '../algebraic-graphs/dag.js'
/* IMPT: Cannot currently use $lib for the following import,
because of how the functions were defined */
import { vertex, overlay } from '../algebraic-graphs/dag.js'

import { match } from 'ts-pattern'

/***********************************
        Lir Data Sources
************************************/

export const VizDeclLirSource: LirSource<IRDecl, DeclLirNode> = {
  toLir(nodeInfo: LirNodeInfo, decl: IRDecl): DeclLirNode {
    return new FunDeclLirNode(
      nodeInfo,
      decl.name,
      decl.params,
      LadderGraphLirSource.toLir(nodeInfo, decl.body)
    )
  },
}

/** Converts IRExpr to a Dag<LirId> (and sets the final dag in the LirContext)
*
* Preconditions / assumptions:
* - To reduce visual clutter in the generated graph, the input ANDs/ORs should
    have already been 'flattened'; i.e.,
    nested exprs like (AND [a (AND [b])]) should have already been flattened to (AND [a b]).
*
*/
export const LadderGraphLirSource: LirSource<IRExpr, LadderGraphLirNode> = {
  toLir(nodeInfo: LirNodeInfo, expr: IRExpr): LadderGraphLirNode {
    // 1. Get structure of the graph
    const overallSource = vertex(new SourceNoAnnoLirNode(nodeInfo).getId())
    const overallSink = vertex(new SinkLirNode(nodeInfo).getId())

    const { graph: middle, vizExprToLir } = transform(nodeInfo, new Map(), expr)

    const dag = overallSource
      .connect(middle.getSource())
      .overlay(middle)
      .overlay(middle.getSink().connect(overallSink))

    const ladderGraph = new LadderGraphLirNode(nodeInfo, dag, vizExprToLir)

    // 2. Augment with explanatory edge labels (TODO: Not sure this shld happen here)
    augmentEdgesWithExplanatoryLabel(nodeInfo.context, ladderGraph)

    return ladderGraph
  },
}

// TODO2: Attach a group id to the label for the flownode to make it easier to debug
// TODO3: Return the number of groups as metadata in the FlowGraph

export interface ToLirResult {
  graph: DirectedAcyclicGraph<LirId>
  vizExprToLir: Map<IRId, DirectedAcyclicGraph<LirId>>
}

/** Internal helper */
function transform(
  nodeInfo: LirNodeInfo,
  /** vizExprToLirEnv */
  env: Map<IRId, DirectedAcyclicGraph<LirId>>,
  expr: IRExpr
): ToLirResult {
  /*
  One of the insights behind the ladder diagram, in particular, behind Meng's `layman`:
    Reduce visual clutter, by creating dummy source and sink nodes between which to nest the subgraphs, and only connecting those.
    That is, we don't connect nodes in different OR/AND groups --- we only connect the source and sink nodes of each group.

  In more detail, the algorithm implemented below is:

  For AND:
  * For each of the child subgraphs, connect the sink of the previous child to the source of the next child.

  For OR:
  * Sandwich the children between a source and sink vertex.
  * For each of the child subgraphs, connect the source of the child to the source of the overall graph; and likewise with the sinks.
  *
  * In "Scalable Drawing of Nested Directed Acyclic Graphs With Gates and Ports",
  * Thomas Leu calls this kind of sandwiching 'bundling'. Leu's layouter and visualizer
  * can do this kind of bundling automatically; it's a pity
  * we need to do it more manually.
  *
  * TODO: I haven't looked at how Leu does the bundling yet --- there might be useful info there too.
  */

  return match(expr)
    .with({ $type: 'BoolVar' }, (originalVar) => {
      const boolvar = new BoolVarLirNode(nodeInfo, originalVar)
      const graph = vertex(boolvar.getId())
      const newEnv = new Map(env).set(originalVar.id, graph)
      return { graph, vizExprToLir: newEnv }
    })
    .with({ $type: 'Not' }, (neg) => {
      const { graph: negand, vizExprToLir: negandEnv } = transform(
        nodeInfo,
        env,
        neg.negand
      )
      const notStart = vertex(new NotStartLirNode(nodeInfo, negand).getId())
      const notEnd = vertex(new NotEndLirNode(nodeInfo).getId())

      const notGraph = notStart
        .connect(negand.getSource())
        .overlay(negand)
        .overlay(negand.getSink().connect(notEnd))
      const newEnv = new Map([...env, ...negandEnv])

      return { graph: notGraph, vizExprToLir: newEnv }
    })
    .with({ $type: 'And' }, (andExpr) => {
      const childResults = andExpr.args.map((arg) =>
        transform(nodeInfo, env, arg)
      )
      const childGraphs = childResults.map((result) => result.graph)

      const combinedGraph = childGraphs.reduceRight((acc, left) => {
        const accSource = acc.getSource()
        const leftSink = left.getSink()
        return leftSink.connect(accSource).overlay(left).overlay(acc)
      })

      const allEnvs = [
        env,
        ...childResults.map((result) => result.vizExprToLir),
      ]
      const combinedEnv = new Map(
        allEnvs.reduceRight(
          (accEntries, env) => [...env, ...accEntries],
          [] as [IRId, DirectedAcyclicGraph<LirId>][]
        )
      )

      return { graph: combinedGraph, vizExprToLir: combinedEnv }
    })
    .with({ $type: 'Or' }, (orExpr) => {
      const childResults = orExpr.args.map((n) => transform(nodeInfo, env, n))
      const childGraphs = childResults.map((result) => result.graph)

      const overallSource = vertex(
        new SourceWithOrAnnoLirNode(
          nodeInfo,
          nodeInfo.context.getOrBundlingNodeLabel()
        ).getId()
      )
      const overallSink = vertex(new SinkLirNode(nodeInfo).getId())

      const leftEdges = childGraphs.map((child) =>
        overallSource.connect(child.getSource())
      )
      const rightEdges = childGraphs.map((child) =>
        child.getSink().connect(overallSink)
      )

      const orGraph = [
        overallSource,
        ...leftEdges,
        ...childGraphs,
        ...rightEdges,
      ].reduce(overlay)

      // Envs
      const childEnvs = childResults.map((result) => result.vizExprToLir)
      const newEnv = new Map(
        childEnvs.reduceRight(
          (entries, env) => [...env, ...entries],
          [] as [IRId, DirectedAcyclicGraph<LirId>][]
        )
      )

      return { graph: orGraph, vizExprToLir: newEnv }
    })
    .exhaustive()
}
