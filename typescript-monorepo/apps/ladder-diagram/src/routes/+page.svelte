<script lang="ts">
  import { fetchJson, sendToLadder } from '$lib/index.js'
  import { fakeJson } from '$lib/fakeJson.js'
  import { onMount } from 'svelte'

  import LadderDiagramComponent from '$lib/LadderDiagram.svelte'

  type LadderData = typeof fakeJson
  let ladderData: LadderData = fakeJson

  // we're using the fake hardcoded json atm
  async function jsonFromApi() {
    try {
      ladderData = await fetchJson((fakeJson))
      // await sendToLadder('', ladderData)
    } catch (error) {
      error = `Error: ${(error as Error).message}`
    }
  }

  onMount(() => {
    jsonFromApi()
  })
</script>

{#if ladderData}
  <LadderDiagramComponent {ladderData} />
{/if}
