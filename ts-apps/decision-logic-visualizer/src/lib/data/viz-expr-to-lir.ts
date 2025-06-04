import type { FunDecl, IRExpr, IRId } from '@repo/viz-expr'
/*
Do not use $lib for the layout-ir imports
*/
import type { LirId, LirNodeInfo } from '../layout-ir/core.js'
import type { LadderLirSource } from '../layout-ir/ladder-lir-source.js'
import type { LadderEnv } from '$lib/ladder-env.js'
import {
  FunDeclLirNode,
  UBoolVarLirNode,
  NotStartLirNode,
  NotEndLirNode,
  SourceNoAnnoLirNode,
  SourceWithOrAnnoLirNode,
  SinkLirNode,
  LadderGraphLirNode,
  augmentEdgesWithExplanatoryLabel,
  AppLirNode,
  TrueExprLirNode,
  FalseExprLirNode,
} from '../layout-ir/ladder-graph/ladder.svelte.js'
import type { DirectedAcyclicGraph, Vertex } from '../algebraic-graphs/dag.js'
/* IMPT: Cannot currently use $lib for the following import,
because of how the functions were defined */
import { vertex, overlays } from '../algebraic-graphs/dag.js'

import { match } from 'ts-pattern'

/***********************************
        Lir Data Sources
************************************/

export const VizDeclLirSource: LadderLirSource<FunDecl, FunDeclLirNode> = {
  async toLir(
    nodeInfo: LirNodeInfo,
    env: LadderEnv,
    decl: FunDecl
  ): Promise<FunDeclLirNode> {
    const ladderGraph = await LadderGraphLirSource.toLir(
      nodeInfo,
      env,
      decl.body
    )
    return new FunDeclLirNode(nodeInfo, decl.name, decl.params, ladderGraph)
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
export const LadderGraphLirSource: LadderLirSource<IRExpr, LadderGraphLirNode> =
  {
    async toLir(
      nodeInfo: LirNodeInfo,
      env: LadderEnv,
      expr: IRExpr
    ): Promise<LadderGraphLirNode> {
      // 1. Get structure of the graph
      const overallSource = vertex(new SourceNoAnnoLirNode(nodeInfo).getId())
      const overallSink = vertex(new SinkLirNode(nodeInfo).getId())
      const {
        graph: middle,
        vizExprToLirGraph,
        noIntermediateBundlingNodeGraph,
      } = transform(nodeInfo, new Map(), expr)
      const dag = sandwichWithSourceAndSink(overallSource, overallSink, middle)
      vizExprToLirGraph.set(expr.id, dag)

      const finalNoIntermediateBundlingNodeGraph = overallSource
        .connect(noIntermediateBundlingNodeGraph)
        .connect(overallSink)

      const ladderGraph = await LadderGraphLirNode.make(
        nodeInfo,
        dag,
        vizExprToLirGraph,
        finalNoIntermediateBundlingNodeGraph,
        expr,
        env
      )

      // 2. Augment with explanatory edge labels (TODO: Not sure this shld happen here)
      augmentEdgesWithExplanatoryLabel(nodeInfo.context, ladderGraph)

      return ladderGraph
    },
  }

// TODO2: Attach a group id to the label for the flownode to make it easier to debug
// TODO3: Return the number of groups as metadata in the FlowGraph

export interface ToLirResult {
  graph: DirectedAcyclicGraph<LirId>
  /** TODO: Would be better to use a branded type over the underlying number for IRId, instead of the IRId wrapper */
  vizExprToLirGraph: Map<IRId, DirectedAcyclicGraph<LirId>>
  noIntermediateBundlingNodeGraph: DirectedAcyclicGraph<LirId>
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
    .with({ $type: 'UBoolVar' }, (originalVar) => {
      const uboolvar = new UBoolVarLirNode(nodeInfo, originalVar)
      const graph = vertex(uboolvar.getId())
      const newEnv = new Map(env).set(originalVar.id, graph)
      return {
        graph,
        vizExprToLirGraph: newEnv,
        noIntermediateBundlingNodeGraph: graph,
      }
    })
    .with({ $type: 'Not' }, (neg) => {
      const {
        graph: negand,
        vizExprToLirGraph: negandEnv,
        noIntermediateBundlingNodeGraph: negandNoIntermediateBundlingNodeGraph,
      } = transform(nodeInfo, env, neg.negand)

      // Make the NOT subgraph
      const notStart = vertex(new NotStartLirNode(nodeInfo, negand).getId())
      const notEnd = vertex(new NotEndLirNode(nodeInfo).getId())

      const notGraph = sandwichWithSourceAndSink(notStart, notEnd, negand)
      const noIntermediateBundlingNodeGraph = sandwichWithSourceAndSink(
        notStart,
        notEnd,
        negandNoIntermediateBundlingNodeGraph
      )

      // Combine the envs
      const combinedEnv = new Map([...env, ...negandEnv]).set(neg.id, notGraph)

      return {
        graph: notGraph,
        vizExprToLirGraph: combinedEnv,
        noIntermediateBundlingNodeGraph,
      }
    })
    .with({ $type: 'And' }, (andExpr) => {
      const childResults = andExpr.args.map((arg) =>
        transform(nodeInfo, env, arg)
      )
      const childGraphs = childResults.map((result) => result.graph)
      const childNoIntermediateBundlingNodeGraphs = childResults.map(
        (result) => result.noIntermediateBundlingNodeGraph
      )

      // Make the AND subgraph
      const makeAndGraph = (
        left: DirectedAcyclicGraph<LirId>,
        right: DirectedAcyclicGraph<LirId>
      ) => {
        const rightSource = right.getSource()
        const leftSink = left.getSink()
        return leftSink.connect(rightSource).overlay(left).overlay(right)
      }
      const makeAndForNoIntermediateBN = (
        left: DirectedAcyclicGraph<LirId>,
        right: DirectedAcyclicGraph<LirId>
      ) => {
        return left.connect(right)
      }

      const combinedGraph = childGraphs.reduce(makeAndGraph)
      const noIntermediateBundlingNodeGraph =
        childNoIntermediateBundlingNodeGraphs.reduce(makeAndForNoIntermediateBN)

      // Combine envs from all child transformations
      const allEnvs = [
        env,
        ...childResults.map((result) => result.vizExprToLirGraph),
      ]
      const combinedEnv = combineEnvs(allEnvs).set(andExpr.id, combinedGraph)

      return {
        graph: combinedGraph,
        vizExprToLirGraph: combinedEnv,
        noIntermediateBundlingNodeGraph,
      }
    })
    .with({ $type: 'Or' }, (orExpr) => {
      const childResults = orExpr.args.map((n) => transform(nodeInfo, env, n))
      const childGraphs = childResults.map((result) => result.graph)
      const childNoIntermediateBundlingNodeGraphs = childResults.map(
        (result) => result.noIntermediateBundlingNodeGraph
      )

      // Make the OR subgraph
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

      const orGraph = overlays([
        overallSource,
        ...leftEdges,
        ...childGraphs,
        ...rightEdges,
      ])

      // Combine envs from all child transformations
      const childEnvs = childResults.map((result) => result.vizExprToLirGraph)
      const combinedEnv = combineEnvs([env, ...childEnvs]).set(
        orExpr.id,
        orGraph
      )

      // noIntermediateBundlingNodeGraph
      const noIntermediateBundlingNodeGraph = overlays(
        childNoIntermediateBundlingNodeGraphs
      )

      return {
        graph: orGraph,
        vizExprToLirGraph: combinedEnv,
        noIntermediateBundlingNodeGraph,
      }
    })
    .with({ $type: 'App' }, (app) => {
      console.log(
        'Note: the App Ladder node currently only supports UBoolVar arguments'
      )
      const childResults = app.args
        .filter((arg) => arg.$type === 'UBoolVar')
        .map((arg) => transform(nodeInfo, env, arg))

      // Get the transformed arg lir nodes
      const argNodes = childResults
        .map((result) => (result.graph as Vertex<LirId>).getValue())
        .map((id) => nodeInfo.context.get(id) as UBoolVarLirNode)

      // Make the App node and its graph
      const appNode = new AppLirNode(nodeInfo, app.fnName, argNodes)
      const appGraph = vertex(appNode.getId())

      // Combine envs from all child transformations
      const childEnvs = childResults.map((result) => result.vizExprToLirGraph)
      const combinedEnv = combineEnvs([env, ...childEnvs]).set(app.id, appGraph)

      return {
        graph: appGraph,
        vizExprToLirGraph: combinedEnv,
        noIntermediateBundlingNodeGraph: appGraph,
      }
    })
    .with({ $type: 'TrueE' }, (trueExpr) => {
      const graph = vertex(new TrueExprLirNode(nodeInfo, trueExpr.name).getId())
      const vizExprToLirGraph = new Map(env).set(trueExpr.id, graph)
      return {
        graph,
        vizExprToLirGraph,
        noIntermediateBundlingNodeGraph: graph,
      }
    })
    .with({ $type: 'FalseE' }, (falseExpr) => {
      const graph = vertex(
        new FalseExprLirNode(nodeInfo, falseExpr.name).getId()
      )
      const vizExprToLirGraph = new Map(env).set(falseExpr.id, graph)
      return {
        graph,
        vizExprToLirGraph,
        noIntermediateBundlingNodeGraph: graph,
      }
    })
    .exhaustive()
}

/****************************
     Helper functions
 ****************************/

/** Helper to combine multiple environments into one */
function combineEnvs(
  envs: Map<IRId, DirectedAcyclicGraph<LirId>>[]
): Map<IRId, DirectedAcyclicGraph<LirId>> {
  return new Map(
    envs.reduceRight(
      (accEntries, env) => [...env, ...accEntries],
      [] as [IRId, DirectedAcyclicGraph<LirId>][]
    )
  )
}

function sandwichWithSourceAndSink(
  overallSource: Vertex<LirId>,
  overallSink: Vertex<LirId>,
  middle: DirectedAcyclicGraph<LirId>
) {
  return overallSource
    .connect(middle.getSource())
    .overlay(middle)
    .overlay(middle.getSink().connect(overallSink))
}
