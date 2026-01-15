<script lang="ts">
  import type { Ladder, LadderNode } from '@repo/decision-service-types'

  type Props = {
    ladder: Ladder | null
    bindings?: Record<string, unknown>
    atomToKeys?: Map<number, string[]>
    onNodeClick?: (unique: number, currentValue: unknown) => void
  }

  let {
    ladder,
    bindings = {},
    atomToKeys = new Map(),
    onNodeClick,
  }: Props = $props()

  interface TreeNode {
    type: string
    label: string
    children: TreeNode[]
    depth: number
    paramKey?: string // For variable nodes (label-based, for display)
    unique?: number // Atom unique ID for binding lookup
  }

  function buildTree(node: LadderNode, depth: number = 0): TreeNode {
    switch (node.$type) {
      case 'And':
        return {
          type: 'And',
          label: 'ALL of',
          children: node.args.map((arg) => buildTree(arg, depth + 1)),
          depth,
        }

      case 'Or':
        return {
          type: 'Or',
          label: 'ANY of',
          children: node.args.map((arg) => buildTree(arg, depth + 1)),
          depth,
        }

      case 'Not':
        return {
          type: 'Not',
          label: 'NOT',
          children: [buildTree(node.negand, depth + 1)],
          depth,
        }

      case 'UBoolVar': {
        const paramKey = node.name.label.replace(/^`|`$/g, '')
        return {
          type: 'Var',
          label: paramKey,
          children: [],
          depth,
          paramKey,
          unique: node.name.unique,
        }
      }

      case 'App': {
        // Function application node - display as a named function with its arguments
        const fnLabel = node.fnName.label.replace(/^`|`$/g, '')
        return {
          type: 'App',
          label: fnLabel,
          children: node.args.map((arg) => buildTree(arg, depth + 1)),
          depth,
          paramKey: fnLabel,
          unique: node.fnName.unique,
        }
      }
    }
  }

  let tree = $derived(ladder ? buildTree(ladder.funDecl.body) : null)

  /**
   * Get the combined value for an atom based on its binding keys.
   * For atoms with multiple binding keys (AND semantics):
   * - True if ALL keys are true
   * - False if ANY key is false
   * - Unknown otherwise
   */
  function getAtomValue(unique?: number): boolean | undefined {
    if (unique === undefined) return undefined

    const keys = atomToKeys.get(unique)
    // DEBUG: Log what we're looking up
    console.log('[LadderDiagram] getAtomValue', {
      unique,
      keys,
      bindings,
      atomToKeysSize: atomToKeys.size,
    })
    if (!keys || keys.length === 0) return undefined

    let hasTrue = false
    let hasFalse = false
    let hasUndefined = false

    for (const key of keys) {
      const value = bindings[key]
      if (value === true) hasTrue = true
      else if (value === false) hasFalse = true
      else hasUndefined = true
    }

    // If any binding is false, the AND is false
    if (hasFalse) return false
    // If all bindings are true, the AND is true
    if (hasTrue && !hasUndefined) return true
    // Otherwise unknown
    return undefined
  }

  function getNodeColor(
    type: string,
    paramKey?: string,
    unique?: number
  ): string {
    if ((type === 'Var' || type === 'App') && unique !== undefined) {
      const value = getAtomValue(unique)
      if (value === true) {
        return 'bg-green-200 border-green-500 text-green-900'
      } else if (value === false) {
        return 'bg-red-200 border-red-500 text-red-900'
      }
      return 'bg-gray-100 border-gray-400 text-gray-700'
    }

    switch (type) {
      case 'And':
        return 'bg-blue-100 border-blue-400 text-blue-800'
      case 'Or':
        return 'bg-green-100 border-green-400 text-green-800'
      case 'Not':
        return 'bg-red-100 border-red-400 text-red-800'
      case 'App':
        return 'bg-purple-100 border-purple-400 text-purple-800'
      default:
        return 'bg-gray-100 border-gray-300 text-gray-700'
    }
  }

  function getValueLabel(unique?: number): string {
    const value = getAtomValue(unique)
    if (value === true) return '✓ True'
    if (value === false) return '✗ False'
    return '? Unknown'
  }

  function handleNodeClick(node: TreeNode) {
    if (
      (node.type === 'Var' || node.type === 'App') &&
      node.unique !== undefined &&
      onNodeClick
    ) {
      // Use unique ID to get the combined value for this atom
      const currentValue = getAtomValue(node.unique)
      onNodeClick(node.unique, currentValue)
    }
  }

  function getIndent(depth: number): string {
    return `${depth * 24}px`
  }
</script>

{#if tree}
  <div class="rounded-lg border-2 border-gray-200 bg-white p-4">
    <h3 class="mb-3 text-sm font-semibold text-gray-700">
      Decision Logic Structure
    </h3>

    <div class="space-y-2">
      {#snippet renderNode(node: TreeNode)}
        <div style="margin-left: {getIndent(node.depth)}" class="space-y-2">
          {#if node.type === 'Not' && node.children.length === 1}
            <!-- Special case: NOT is unary, render inline with child -->
            <div class="inline-flex items-center gap-2">
              <span
                class="inline-flex items-center gap-1 rounded border-2 border-red-400 bg-red-100 px-2 py-1 text-xs font-medium text-red-800"
              >
                <svg
                  class="h-3 w-3"
                  fill="none"
                  stroke="currentColor"
                  viewBox="0 0 24 24"
                >
                  <path
                    stroke-linecap="round"
                    stroke-linejoin="round"
                    stroke-width="2"
                    d="M6 18L18 6M6 6l12 12"
                  />
                </svg>
                NOT
              </span>
              {#if node.children[0]}
                {@const child = node.children[0]}
                <!-- svelte-ignore a11y_no_noninteractive_tabindex -->
                <div
                  class="inline-flex items-center gap-2 rounded border-2 px-3 py-1.5 text-sm font-medium {getNodeColor(
                    child.type,
                    child.paramKey,
                    child.unique
                  )} {child.type === 'Var' && onNodeClick
                    ? 'cursor-pointer hover:shadow-md transition-shadow'
                    : ''}"
                  onclick={() => handleNodeClick(child)}
                  role={child.type === 'Var' && onNodeClick
                    ? 'button'
                    : undefined}
                  tabindex={child.type === 'Var' && onNodeClick ? 0 : -1}
                >
                  {#if child.type === 'And'}
                    <svg
                      class="h-4 w-4"
                      fill="none"
                      stroke="currentColor"
                      viewBox="0 0 24 24"
                    >
                      <path
                        stroke-linecap="round"
                        stroke-linejoin="round"
                        stroke-width="2"
                        d="M12 4v16m8-8H4"
                      />
                    </svg>
                  {:else if child.type === 'Or'}
                    <svg
                      class="h-4 w-4"
                      fill="none"
                      stroke="currentColor"
                      viewBox="0 0 24 24"
                    >
                      <circle cx="12" cy="12" r="10" stroke-width="2" />
                      <path
                        stroke-linecap="round"
                        stroke-linejoin="round"
                        stroke-width="2"
                        d="M8 12h8"
                      />
                    </svg>
                  {/if}
                  <span>{child.label}</span>
                  {#if (child.type === 'Var' || child.type === 'App') && child.unique !== undefined}
                    <span class="ml-2 text-xs font-semibold">
                      {getValueLabel(child.unique)}
                    </span>
                  {/if}
                </div>
              {/if}
            </div>
          {:else}
            <!-- svelte-ignore a11y_no_noninteractive_tabindex -->
            <div
              class="inline-flex items-center gap-2 rounded border-2 px-3 py-1.5 text-sm font-medium {getNodeColor(
                node.type,
                node.paramKey,
                node.unique
              )} {(node.type === 'Var' || node.type === 'App') && onNodeClick
                ? 'cursor-pointer hover:shadow-md transition-shadow'
                : ''}"
              onclick={() => handleNodeClick(node)}
              role={(node.type === 'Var' || node.type === 'App') && onNodeClick
                ? 'button'
                : undefined}
              tabindex={(node.type === 'Var' || node.type === 'App') &&
              onNodeClick
                ? 0
                : -1}
            >
              {#if node.type === 'And'}
                <svg
                  class="h-4 w-4"
                  fill="none"
                  stroke="currentColor"
                  viewBox="0 0 24 24"
                >
                  <path
                    stroke-linecap="round"
                    stroke-linejoin="round"
                    stroke-width="2"
                    d="M12 4v16m8-8H4"
                  />
                </svg>
              {:else if node.type === 'Or'}
                <svg
                  class="h-4 w-4"
                  fill="none"
                  stroke="currentColor"
                  viewBox="0 0 24 24"
                >
                  <circle cx="12" cy="12" r="10" stroke-width="2" />
                  <path
                    stroke-linecap="round"
                    stroke-linejoin="round"
                    stroke-width="2"
                    d="M8 12h8"
                  />
                </svg>
              {:else if node.type === 'App'}
                <svg
                  class="h-4 w-4"
                  fill="none"
                  stroke="currentColor"
                  viewBox="0 0 24 24"
                >
                  <path
                    stroke-linecap="round"
                    stroke-linejoin="round"
                    stroke-width="2"
                    d="M9 5H7a2 2 0 00-2 2v12a2 2 0 002 2h10a2 2 0 002-2V7a2 2 0 00-2-2h-2M9 5a2 2 0 002 2h2a2 2 0 002-2M9 5a2 2 0 012-2h2a2 2 0 012 2"
                  />
                </svg>
              {/if}
              <span>{node.label}</span>
              {#if (node.type === 'Var' || node.type === 'App') && node.unique !== undefined}
                <span class="ml-2 text-xs font-semibold">
                  {getValueLabel(node.unique)}
                </span>
              {/if}
            </div>

            {#if node.children.length > 0 && node.type !== 'Not' && node.type !== 'Var'}
              <div class="space-y-2">
                {#each node.children as child}
                  {@render renderNode(child)}
                {/each}
              </div>
            {/if}
          {/if}
        </div>
      {/snippet}

      {@render renderNode(tree)}
    </div>
  </div>
{:else}
  <div
    class="rounded-lg border-2 border-dashed border-gray-300 bg-gray-50 p-8 text-center text-sm text-gray-500"
  >
    No decision logic structure available
  </div>
{/if}
