import type { QueryPlanResponse } from '@repo/decision-service-types'
import { schemaSummary } from '@repo/decision-service-types'
import type { LirContext } from '@repo/layout-ir'
import type { Unique } from '@repo/viz-expr'
import type { ElicitationAsk, PartialEvalAnalysis } from './partial-eval.js'

type AtomLike = {
  atomId: string
  label: string
}

type LadderGraphAtomLookup = {
  getUniquesForAtomId(context: LirContext, atomId: string): Unique[]
  getUniquesForLabel(context: LirContext, label: string): Unique[]
}

export type ElicitationOverride = {
  ranked: Unique[]
  stillNeeded: Unique[]
  next: Unique[]
  askByUnique: PartialEvalAnalysis['askByUnique']
}

function uniquesForAtom(
  context: LirContext,
  ladderGraph: LadderGraphAtomLookup,
  atom: AtomLike
): Unique[] {
  const byAtomId = ladderGraph.getUniquesForAtomId(context, atom.atomId)
  return byAtomId.length > 0
    ? byAtomId
    : ladderGraph.getUniquesForLabel(context, atom.label)
}

function uniquesForAtoms(
  context: LirContext,
  ladderGraph: LadderGraphAtomLookup,
  atoms: AtomLike[]
): Unique[] {
  const out: Unique[] = []
  const seen = new Set<Unique>()
  for (const atom of atoms) {
    for (const u of uniquesForAtom(context, ladderGraph, atom)) {
      if (seen.has(u)) continue
      seen.add(u)
      out.push(u)
    }
  }
  return out
}

export function elicitationOverrideFromQueryPlan(
  context: LirContext,
  ladderGraph: LadderGraphAtomLookup,
  resp: QueryPlanResponse
): ElicitationOverride {
  const askByUnique = new Map<Unique, ElicitationAsk[]>()
  const seenAskByUnique = new Map<Unique, Set<string>>()

  for (const ask of resp.asks) {
    for (const atom of ask.atoms) {
      for (const u of uniquesForAtom(context, ladderGraph, atom)) {
        const seen = seenAskByUnique.get(u) ?? new Set<string>()
        seenAskByUnique.set(u, seen)
        if (seen.has(ask.label)) continue
        seen.add(ask.label)

        const entry = askByUnique.get(u) ?? []
        askByUnique.set(u, entry)
        entry.push({
          container: ask.container,
          path: ask.path,
          label: ask.label,
          schemaSummary: schemaSummary(ask.schema),
        })
      }
    }
  }

  const rankedAtoms =
    resp.asks.length > 0 ? resp.asks.flatMap((a) => a.atoms) : resp.ranked

  return {
    ranked: uniquesForAtoms(context, ladderGraph, rankedAtoms),
    stillNeeded: uniquesForAtoms(context, ladderGraph, resp.stillNeeded),
    next:
      resp.asks.length > 0
        ? uniquesForAtoms(context, ladderGraph, resp.asks[0]!.atoms)
        : [],
    askByUnique,
  }
}
