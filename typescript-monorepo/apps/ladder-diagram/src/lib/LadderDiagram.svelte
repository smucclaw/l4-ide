<script lang="ts">
  import { onMount, afterUpdate } from 'svelte'
  import {
    BoolVar,
    AllQuantifier,
    AnyQuantifier,
    LadderDiagram,
  } from 'ladder-diagram/ladder.js'

  export let ladderData

  let ladderHere: HTMLDivElement
  let ld: typeof LadderDiagram

  type Mark = {
    value: 'undefined' | 'true' | 'false'
    source: 'default' | 'user'
  }

  type AndOrLeaf = {
    tag: 'Leaf'
    nl: Record<string, unknown>
    contents: string
  }

  type AndOrQuantifier = {
    tag: 'All' | 'Any'
    nl: Record<string, unknown>
    children: LadderDataLeaf[]
  }

  type LadderDataLeaf = {
    andOr: AndOrLeaf | AndOrQuantifier
    mark: Mark
    prePost?: {
      Pre?: string
    }
    shouldView: 'Ask' | 'View'
  }

  type CircuitType =
    | typeof BoolVar
    | typeof AllQuantifier
    | typeof AnyQuantifier

  function q2circuit(q: LadderDataLeaf): CircuitType {
    if (q.andOr.tag === 'Leaf') {
      const utf =
        q.mark.value === 'undefined'
          ? 'U'
          : q.mark.value === 'true'
            ? 'T'
            : q.mark.value === 'false'
              ? 'F'
              : null
      return new BoolVar(
        q.andOr.contents,
        false,
        q.mark.source === 'default' ? utf : null,
        q.mark.source === 'user' ? utf : null
      )
    }
    const Construct = q.andOr.tag === 'All' ? AllQuantifier : AnyQuantifier
    return new Construct(q.andOr.children.map((c): CircuitType => q2circuit(c)))
  }

  onMount(() => {
    const circuit = q2circuit(ladderData)
    ld = new LadderDiagram(circuit)
    ld.attach(ladderHere)
  })

  // holder function for updating data
  // afterUpdate(() => {
  //   if (ld) {
  //     ladderHere.innerHTML = ''
  //     ld.attach(ladderHere)
  //   }
  // })
</script>

<link rel="stylesheet" href="./node_modules/ladder-diagram/css/ladder.css" />

<div bind:this={ladderHere}></div>
