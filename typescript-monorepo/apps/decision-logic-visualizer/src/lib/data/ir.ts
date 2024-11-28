import { Schema } from 'effect'

/**********************
         IR
************************

The following is pretty much adapted / copied from the interfaces sketched at
https://github.com/smucclaw/lam4/blob/main/experiments/ide-interfaces/l4extensionprototype/src/interfaces/decisionLogicIRNode.ts

The main difference is that I'm experimenting with using Effect schemas,
in order to get functionality like serialization/deserialization, generation of the corresponding JSON Schemas, and better error messages.

-----------------
  Design intent
-----------------

I framed the visualizer inputs in terms of PL concepts,
as opposed to 'lower-level' graph visualization concepts (e.g. overlaying and connecting nodes),
because my understanding was that Matthew Waddington wanted
the produced components to be things that other legal DSLs can also benefit from.
In particular, he seemed to want something like
a 'L4-lite' intermediate representation whose semantics was mostly confined to propositional logic,
and that other legal DSLs can either compile to (so as, e.g., to use tools whose inputs
are in terms of this intermediate representation)
or extend with their own features.

Furthermore, if the visualizer inputs were framed in terms of, e.g., 'lower-level' graph visualization concepts,
that wouldn't be very different from visualization tools that already exist for visualizing graphs.

The other design goal was to try to come up with something that's simple while still being relatively extensible.
If there are things that do not seem easily extensible, please do not hesitate to point those out.

---------------------------------------
  Relationship to previous prototypes
---------------------------------------
In coming up with the following, I
* looked through all the visualization-related components we've previously done (mathlangvis, Jules' ladder diagram, Meng's layman, vue pure pdpa)
and did a quick personal assessment of their software interfaces, design intents,
and practical pros and cons when it came to re-using them for this usecase.
 (In particular, I've run and played with most of them.)
* also looked at Maryam's https://github.com/Meowyam/lspegs

If it seems like there are aspects of extant work that have not been considered in the following,
it's likely because they are either not needed for the v1 mvp or looked like they'd have other practical disadvantages.
*/

/***************************
  Common types and schemas
****************************/

/*
Note: We need some type declarations, in addition to the Effect schemas,
in order to implement recursive and mutually recursive schemas.
See https://effect.website/docs/schema/advanced-usage/#recursive-schemas
*/

/** A separate record type for annotations makes it easy to add more annotation types in the future */
export const IRNodeAnnotation = Schema.Struct({
  label: Schema.optional(Schema.String),
})

/** Stable IDs useful for things like bidirectional synchronization down the road */
export const IRId = Schema.Struct({
  id: Schema.Number,
})

/** Stable IDs useful for things like bidirectional synchronization down the road */
interface IRId {
  readonly id: number
}

export interface IRNodeAnnotation {
  /** The label is what gets displayed in or around the box. */
  readonly label?: string
}

/*****************
  Core IR node
*****************/

export interface IRNode {
  /** Discriminating property for IRNodes */
  readonly $type: string
  /** (Stable) ID for this IRNode */
  readonly id: IRId
  readonly annotation: IRNodeAnnotation
}

export const IRNode = Schema.Struct({
  $type: Schema.String,
  id: IRId,
  annotation: IRNodeAnnotation,
})

/*******************************
  Decision Logic (ish) IR node
********************************/

export type IRExpr = BinExpr | Not | AtomicProposition

export type BinOp = 'And' | 'Or'

export interface BinExpr extends IRNode {
  readonly $type: 'BinExpr'
  readonly op: BinOp
  readonly left: IRExpr
  readonly right: IRExpr
}

export interface Not extends IRNode {
  readonly $type: 'Not'
  readonly value: IRExpr
}

export interface AtomicProposition extends IRNode {
  readonly $type: 'AtomicProposition'
  readonly value: 'False' | 'True' | 'Unknown'
  readonly annotation: AtomicPropositionAnnotation
}

/**
I can't think of a scenario where we'd plausibly want atomic propositions in something like a ladder diagram to not have a label;
and conversely it is easy to think of scenarios where one forgets to add the label for atomic propositions.
*/
export interface AtomicPropositionAnnotation extends IRNodeAnnotation {
  readonly label: string
}

/*---------------------------------
  The corresponding Effect Schemas
  ---------------------------------*/

export const IRExpr = Schema.Union(
  Schema.suspend((): Schema.Schema<BinExpr> => BinExpr),
  Schema.suspend((): Schema.Schema<Not> => Not),
  Schema.suspend((): Schema.Schema<AtomicProposition> => AtomicProposition)
)

export const BinOp = Schema.Union(Schema.Literal('And'), Schema.Literal('Or'))

export const BinExpr = Schema.Struct({
  $type: Schema.tag('BinExpr'),
  op: BinOp,
  left: IRExpr,
  right: IRExpr,
  id: IRId,
  annotation: IRNodeAnnotation,
})

export const Not = Schema.Struct({
  $type: Schema.tag('Not'),
  value: IRExpr,
  id: IRId,
  annotation: IRNodeAnnotation,
})

export const AtomicPropositionAnnotation = Schema.Struct({
  label: Schema.String,
})

export const AtomicProposition = Schema.Struct({
  $type: Schema.tag('AtomicProposition'),
  value: Schema.Union(
    Schema.Literal('False'),
    Schema.Literal('True'),
    Schema.Literal('Unknown')
  ),
  id: IRId,
  annotation: AtomicPropositionAnnotation,
})

/***********************************
  Wrapper / Protocol interfaces
************************************/

export const VisualizeDecisionLogicIRInfo = Schema.Struct({
  program: IRExpr,
})

export const VisualizeDecisionLogicResult = Schema.Struct({
  html: Schema.String,
})
