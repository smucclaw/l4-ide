/**
 * Ladder logic - evaluates the boolean decision logic tree
 * from the L4 decision service using three-valued logic
 */

import type { Ladder, LadderNode } from '@repo/decision-service-types'

/**
 * Three-valued logic for evaluating boolean expressions
 */
export enum Ternary {
  True = 'True',
  False = 'False',
  Unknown = 'Unknown',
}

/**
 * Evaluate a ladder node given current parameter bindings
 */
export function evaluateLadder(
  node: LadderNode,
  atomValues: Map<number, Ternary>
): Ternary {
  switch (node.$type) {
    case 'UBoolVar': {
      return atomValues.get(node.name.unique) ?? Ternary.Unknown
    }

    case 'And': {
      const results = node.args.map((arg) => evaluateLadder(arg, atomValues))
      // AND: true if all true, false if any false, unknown otherwise
      if (results.every((r) => r === Ternary.True)) {
        return Ternary.True
      }
      if (results.some((r) => r === Ternary.False)) {
        return Ternary.False
      }
      return Ternary.Unknown
    }

    case 'Or': {
      const results = node.args.map((arg) => evaluateLadder(arg, atomValues))
      // OR: true if any true, false if all false, unknown otherwise
      if (results.some((r) => r === Ternary.True)) {
        return Ternary.True
      }
      if (results.every((r) => r === Ternary.False)) {
        return Ternary.False
      }
      return Ternary.Unknown
    }

    case 'Not': {
      const result = evaluateLadder(node.arg, atomValues)
      if (result === Ternary.True) return Ternary.False
      if (result === Ternary.False) return Ternary.True
      return Ternary.Unknown
    }
  }
}

/**
 * Determine if a subtree is relevant given the parent's value and current bindings.
 * This implements short-circuit logic:
 * - In an OR, if one branch is True, other branches become irrelevant
 * - In an AND, if one branch is False, other branches become irrelevant
 */
export function isRelevant(
  node: LadderNode,
  parentValue: Ternary,
  selfValue: Ternary
): boolean {
  // If parent determined the answer, children don't matter
  if (parentValue === Ternary.True || parentValue === Ternary.False) {
    // But we need to know: did this child contribute to that determination?
    // If selfValue differs from what parent needs, this is irrelevant
    return selfValue === Ternary.Unknown
  }

  // Parent is unknown, so we're potentially relevant
  return true
}

/**
 * Extract all atom unique IDs from a ladder node tree
 */
export function extractAtomIds(node: LadderNode): Set<number> {
  const ids = new Set<number>()

  function traverse(n: LadderNode) {
    switch (n.$type) {
      case 'UBoolVar':
        ids.add(n.name.unique)
        break
      case 'And':
      case 'Or':
        n.args.forEach(traverse)
        break
      case 'Not':
        traverse(n.arg)
        break
    }
  }

  traverse(node)
  return ids
}

/**
 * Find the logical context (parent AND/OR node) for each atom ID
 */
export interface AtomContext {
  atomId: number
  parentType: 'And' | 'Or' | 'Root' | 'Not'
  siblings: number[] // Other atoms in the same parent context
}

export function buildAtomContexts(ladder: Ladder): Map<number, AtomContext> {
  const contexts = new Map<number, AtomContext>()

  function traverse(
    node: LadderNode,
    parentType: 'And' | 'Or' | 'Root' | 'Not'
  ) {
    switch (node.$type) {
      case 'UBoolVar': {
        const atomId = node.name.unique
        if (!contexts.has(atomId)) {
          contexts.set(atomId, {
            atomId,
            parentType,
            siblings: [],
          })
        }
        break
      }

      case 'And':
      case 'Or': {
        const siblings = node.args
          .filter((arg) => arg.$type === 'UBoolVar')
          .map((arg) => (arg as BoolVarNode).name.unique)

        node.args.forEach((arg) => {
          if (arg.$type === 'UBoolVar') {
            const atomId = arg.name.unique
            contexts.set(atomId, {
              atomId,
              parentType: node.$type,
              siblings,
            })
          } else {
            traverse(arg, node.$type)
          }
        })
        break
      }

      case 'Not':
        traverse(node.arg, 'Not')
        break
    }
  }

  traverse(ladder.funDecl.body, 'Root')
  return contexts
}

/**
 * Logical group information extracted from the ladder tree
 */
export interface LogicalGroup {
  id: string // Unique identifier for this group
  type: 'And' | 'Or' | 'Root'
  label?: string // Optional human-readable label
  atomIds: Set<number> // Atoms directly in this group
  childGroups: LogicalGroup[] // Nested subgroups
  depth: number // Nesting depth (0 = root)
}

/**
 * Extract logical groups from the ladder tree structure.
 * This creates a hierarchy of groups based on AND/OR nodes.
 */
export function extractLogicalGroups(ladder: Ladder | null): LogicalGroup[] {
  if (!ladder) return []

  let groupIdCounter = 0
  const groups: LogicalGroup[] = []

  function traverse(
    node: LadderNode,
    depth: number,
    parentType: 'And' | 'Or' | 'Root' = 'Root'
  ): LogicalGroup | null {
    switch (node.$type) {
      case 'UBoolVar': {
        // Leaf node - return null, handled by parent
        return null
      }

      case 'And':
      case 'Or': {
        const groupId = `group_${groupIdCounter++}`
        const atomIds = new Set<number>()
        const childGroups: LogicalGroup[] = []

        // Process each argument
        for (const arg of node.args) {
          if (arg.$type === 'UBoolVar') {
            atomIds.add(arg.name.unique)
          } else {
            const childGroup = traverse(arg, depth + 1, node.$type)
            if (childGroup) {
              childGroups.push(childGroup)
            }
          }
        }

        return {
          id: groupId,
          type: node.$type,
          atomIds,
          childGroups,
          depth,
        }
      }

      case 'Not': {
        // For NOT nodes, just traverse through them
        return traverse(node.arg, depth, parentType)
      }
    }
  }

  const rootGroup = traverse(ladder.funDecl.body, 0)
  if (rootGroup) {
    groups.push(rootGroup)
  }

  return groups
}

/**
 * Build a mapping from atom IDs to parameter keys using the query plan
 */
export function buildAtomToParameterMap(
  asks: Array<{ container: string; atoms: Array<{ unique: number }> }>
): Map<number, Set<string>> {
  const atomToParams = new Map<number, Set<string>>()

  for (const ask of asks) {
    const paramKey = ask.container.replace(/^`|`$/g, '') // Strip backticks
    for (const atom of ask.atoms) {
      if (!atomToParams.has(atom.unique)) {
        atomToParams.set(atom.unique, new Set())
      }
      atomToParams.get(atom.unique)!.add(paramKey)
    }
  }

  return atomToParams
}

/**
 * Group parameters based on the ladder structure
 */
export interface ParameterGroup {
  id: string
  label: string
  type: 'And' | 'Or' | 'Root'
  parameters: string[] // Parameter keys
  children: ParameterGroup[]
  depth: number
}

export function groupParametersByLadder(
  ladder: Ladder | null,
  asks: Array<{ container: string; atoms: Array<{ unique: number }> }>
): ParameterGroup[] {
  if (!ladder) return []

  const logicalGroups = extractLogicalGroups(ladder)
  const atomToParams = buildAtomToParameterMap(asks)

  function convertGroup(
    logicalGroup: LogicalGroup,
    index: number
  ): ParameterGroup {
    // Collect all parameters for atoms in this group
    const parameters = new Set<string>()
    for (const atomId of logicalGroup.atomIds) {
      const params = atomToParams.get(atomId)
      if (params) {
        params.forEach((p) => parameters.add(p))
      }
    }

    // Generate a label based on the group type and parameters
    let label = ''
    if (logicalGroup.type === 'Or') {
      label = 'Any of'
    } else if (logicalGroup.type === 'And') {
      label = 'All of'
    } else {
      label = `Group ${index + 1}`
    }

    return {
      id: logicalGroup.id,
      label,
      type: logicalGroup.type,
      parameters: Array.from(parameters),
      children: logicalGroup.childGroups.map((child, i) =>
        convertGroup(child, i)
      ),
      depth: logicalGroup.depth,
    }
  }

  return logicalGroups.map((group, i) => convertGroup(group, i))
}
