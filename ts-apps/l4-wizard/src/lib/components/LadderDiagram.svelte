<script lang="ts">
  import type { Ladder, LadderNode } from '@repo/decision-service-types'

  type Props = {
    ladder: Ladder | null
  }

  let { ladder }: Props = $props()

  interface TreeNode {
    type: string
    label: string
    children: TreeNode[]
    depth: number
  }

  function buildTree(node: LadderNode, depth: number = 0): TreeNode {
    switch (node.$type) {
      case 'And':
        return {
          type: 'And',
          label: 'ALL of',
          children: node.args.map(arg => buildTree(arg, depth + 1)),
          depth
        }

      case 'Or':
        return {
          type: 'Or',
          label: 'ANY of',
          children: node.args.map(arg => buildTree(arg, depth + 1)),
          depth
        }

      case 'Not':
        return {
          type: 'Not',
          label: 'NOT',
          children: [buildTree(node.arg, depth + 1)],
          depth
        }

      case 'UBoolVar':
        return {
          type: 'Var',
          label: node.name.label.replace(/^`|`$/g, ''),
          children: [],
          depth
        }
    }
  }

  let tree = $derived(ladder ? buildTree(ladder.funDecl.body) : null)

  function getNodeColor(type: string): string {
    switch (type) {
      case 'And': return 'bg-blue-100 border-blue-400 text-blue-800'
      case 'Or': return 'bg-green-100 border-green-400 text-green-800'
      case 'Not': return 'bg-red-100 border-red-400 text-red-800'
      case 'Var': return 'bg-gray-100 border-gray-300 text-gray-700'
      default: return 'bg-gray-100 border-gray-300 text-gray-700'
    }
  }

  function getIndent(depth: number): string {
    return `${depth * 24}px`
  }
</script>

{#if tree}
  <div class="rounded-lg border-2 border-gray-200 bg-white p-4">
    <h3 class="mb-3 text-sm font-semibold text-gray-700">Decision Logic Structure</h3>

    <div class="space-y-2">
      {#snippet renderNode(node: TreeNode)}
        <div style="margin-left: {getIndent(node.depth)}" class="space-y-2">
          <div class="inline-flex items-center gap-2 rounded border-2 px-3 py-1.5 text-sm font-medium {getNodeColor(node.type)}">
            {#if node.type === 'And'}
              <svg class="h-4 w-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 4v16m8-8H4" />
              </svg>
            {:else if node.type === 'Or'}
              <svg class="h-4 w-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <circle cx="12" cy="12" r="10" stroke-width="2"/>
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M8 12h8" />
              </svg>
            {:else if node.type === 'Not'}
              <svg class="h-4 w-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12" />
              </svg>
            {/if}
            <span>{node.label}</span>
          </div>

          {#if node.children.length > 0}
            <div class="space-y-2">
              {#each node.children as child}
                {@render renderNode(child)}
              {/each}
            </div>
          {/if}
        </div>
      {/snippet}

      {@render renderNode(tree)}
    </div>
  </div>
{:else}
  <div class="rounded-lg border-2 border-dashed border-gray-300 bg-gray-50 p-8 text-center text-sm text-gray-500">
    No decision logic structure available
  </div>
{/if}
