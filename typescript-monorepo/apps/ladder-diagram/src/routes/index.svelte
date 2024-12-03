<script lang="ts">
  import { sendToLadder } from '$lib/api.js'
  import { fakeJson } from '$lib/fakeJson.js'

  import LadderDiagramComponent from '$lib/LadderDiagram.svelte'

  type LadderData = typeof fakeJson
  let ladderData: LadderData = fakeJson
  console.log('ladderdata', ladderData)

  async function jsonFromApi() {
    try {
      await sendToLadder('', ladderData)
    } catch (error) {
      status = `Error: ${(error as Error).message}`
    }
  }
</script>

{#if ladderData}
  <LadderDiagramComponent {ladderData} />
{/if}
