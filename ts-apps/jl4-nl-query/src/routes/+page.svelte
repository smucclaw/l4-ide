<script lang="ts">
  import { Input } from '$lib/components/ui/input/index.js'
  import { Button } from '$lib/components/ui/button/index.js'

  let query: string = $state('')
</script>

<div class="container w-3/4 mx-auto p-4">
  <h1>Ask JL4</h1>
  <!-- A menu of queries / topics / laws -->
  <section class="mt-5 mb-8 ml-5">
    <h2>I have a question about... [TODO]</h2>
    <ul class="list-disc list-inside space-y-1">
      <li>Companies (Jersey) Law 1991</li>
      <li>Employment (Jersey) Law 2003</li>
      <li>Income Tax (Jersey) Law 1961</li>
      <li>Road Traffic (Jersey) Law 1956</li>
    </ul>
  </section>

  <!-- Input box and submit button -->
  <div class="flex space-x-3">
    <Input
      type="text"
      placeholder="Ask a question about Jersey laws..."
      class="flex-1 px-4 py-2 mb-2 border-2 border-gray-400 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500"
    />
    <Button
      class="px-4 py-2 bg-blue-600 text-white font-semibold rounded-md hover:bg-blue-800 focus:outline-none focus:ring-2 focus:ring-blue-500"
    >
      Submit
    </Button>
  </div>
</div>

<style lang="postcss">
  @reference "tailwindcss";
  h1 {
    @apply mb-4 mt-8 text-4xl font-semibold;
  }

  h2 {
    @apply mb-4 mt-6 text-2xl font-semibold text-stone-500;
  }

  h3 {
    @apply mb-2 mt-4 text-xl font-semibold;
  }
</style>
