import { Schema, Pretty, JSONSchema } from 'effect'

/**********************
      VizExpr IR
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

/***********************************************************
        Name
  (a label / 'RawName' and a Unique for equality of exprs)
*************************************************************/

export const Name = Schema.Struct({
  /** Uniques for checking whether, e.g., two BoolVar IRNodes actually refer to the same proposition.
   *
   * BoolVar IRNodes that refer to the same proposition can nevertheless differ
   * in virtue of eg having different nlg annotations.  */
  unique: Schema.Number,
  label: Schema.String,
})

export interface Name {
  /** For equality of exprs */
  readonly unique: Unique
  /** The label is what gets displayed in or around the box. */
  readonly label: string
}

export type Unique = number

/*******************************
           IRId
********************************/

/** Stable IDs useful for things like bidirectional synchronization down the road */
export const IRId = Schema.Struct({
  id: Schema.Number,
})

/** Stable IDs useful for things like bidirectional synchronization down the road */
export interface IRId {
  readonly id: number
}

/*****************
  Core IR node
*****************/

export interface IRNode {
  /** Discriminating property for IRNodes */
  readonly $type: string
  /** (Stable) ID for this IRNode */
  readonly id: IRId
}

export const IRNode = Schema.Struct({
  $type: Schema.String,
  id: IRId,
})

/*******************************
  Decision Logic (ish) IR node
********************************/

export interface FunDecl extends IRNode {
  readonly $type: 'FunDecl'
  readonly name: Name
  readonly params: readonly Name[] // TODO: Will worry about adding type info later
  readonly body: IRExpr
}

/** Think of this as the Expr for the decision logic.
 * I.e., the Expr type here will likely be a proper subset of the source language's Expr type.
 */
export type IRExpr = And | Or | BoolVar | Not

/* Thanks to Andres for pointing out that an n-ary representation would be better for the arguments for And / Or.
It's one of those things that seems obvious in retrospect; to quote
Brent Yorgey: "The set of booleans forms a monoid under conjunction (with identity True), disjunction (with identity False)" */

/**
* Preconditions / assumptions
* - The visualizer assumes that any nested ANDs/ORs have already been 'flattened'.
    E.g., nested exprs like (AND [a (AND [b])]) should have already been rewritten to (AND [a b]).
    This should be done in order to reduce visual clutter in the generated graph; but the visualizer will not do this sort of flattening.
*/
export interface And extends IRNode {
  readonly $type: 'And'
  readonly args: readonly IRExpr[]
}

/**
* Preconditions / assumptions
* - The visualizer assumes that any nested ANDs/ORs have already been 'flattened'.
    E.g., nested exprs like (AND [a (AND [b])]) should have already been rewritten to (AND [a b]).
    This should be done in order to reduce visual clutter in the generated graph; but the visualizer will not do this sort of flattening.
*/
export interface Or extends IRNode {
  readonly $type: 'Or'
  readonly args: readonly IRExpr[]
}

export interface Not extends IRNode {
  readonly $type: 'Not'
  readonly negand: IRExpr
}

/** For the original Viz / IRExpr */
export type BoolValue = 'False' | 'True' | 'Unknown'

export interface BoolVar extends IRNode {
  readonly $type: 'BoolVar'
  readonly name: Name
  readonly value: BoolValue
}

export type Value = BoolValue

/***********************************
  The corresponding Effect Schemas
************************************/

export const IRExpr = Schema.Union(
  Schema.suspend((): Schema.Schema<And> => And),
  Schema.suspend((): Schema.Schema<Or> => Or),
  Schema.suspend((): Schema.Schema<Not> => Not),
  Schema.suspend((): Schema.Schema<BoolVar> => BoolVar)
).annotations({ identifier: 'IRExpr' })

export const FunDecl = Schema.Struct({
  $type: Schema.tag('FunDecl'),
  id: IRId,
  name: Name,
  params: Schema.Array(Name),
  body: IRExpr,
}).annotations({ identifier: 'FunDecl' })

export const And = Schema.Struct({
  $type: Schema.tag('And'),
  id: IRId,
  args: Schema.Array(IRExpr),
}).annotations({ identifier: 'And' })

export const Or = Schema.Struct({
  $type: Schema.tag('Or'),
  id: IRId,
  args: Schema.Array(IRExpr),
}).annotations({ identifier: 'Or' })

export const Not = Schema.Struct({
  $type: Schema.tag('Not'),
  negand: IRExpr,
  id: IRId,
}).annotations({ identifier: 'Not' })

export const BoolValue = Schema.Union(
  Schema.Literal('False'),
  Schema.Literal('True'),
  Schema.Literal('Unknown')
)

export const BoolVar = Schema.Struct({
  $type: Schema.tag('BoolVar'),
  value: BoolValue,
  id: IRId,
  name: Name,
}).annotations({ identifier: 'BoolVar' })

/***********************************
  Wrapper / Protocol interfaces
************************************/

/** The payload for VisualizeDecisionLogicNotification */
export type VisualizeDecisionLogicIRInfo = Schema.Schema.Type<
  typeof VisualizeDecisionLogicIRInfo
>

export const VisualizeDecisionLogicIRInfo = Schema.Struct({
  funDecl: FunDecl,
}).annotations({ identifier: 'VisualizeDecisionLogicIRInfo' })

/*************************
    Decode
**************************/

export function makeVizInfoDecoder() {
  return Schema.decodeUnknownEither(VisualizeDecisionLogicIRInfo)
}

/***********************************
        Examples of usage
************************************/

// Example of how to use Effect to decode an unknown input

// const decode = Schema.decodeUnknownEither(IRExpr)

/** Example of an unknown input */
// const egAtomicPropWalks = {
//   $type: 'BoolVar',
//   value: 'True',
//   id: { id: 1 },
//   name: { label: 'walks', unique: 2 }
// }

// const result = decode(egAtomicPropWalks)
// console.log(result)

/*************************
    Utils
**************************/

/** See https://effect.website/docs/schema/pretty/ */
export function getDecisionLogicIRPrettyPrinter() {
  return Pretty.make(VisualizeDecisionLogicIRInfo)
}

/** Get a JSON Schema version of the VisualizeDecisionLogicIRInfo */
export function exportDecisionLogicIRInfoToJSONSchema() {
  return JSON.stringify(JSONSchema.make(VisualizeDecisionLogicIRInfo))
}
// console.log(exportDecisionLogicIRInfoToJSONSchema())

/*************************
    JSON Schema version
**************************/

/*

*/
