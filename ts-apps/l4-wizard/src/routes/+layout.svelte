<script lang="ts">
  import '../app.css'
  let { children } = $props()

  const commitHash = __COMMIT_HASH__
</script>

<div class="min-h-screen flex flex-col">
  <div class="flex-1">
    {@render children()}
  </div>
  <footer class="text-center text-xs text-gray-400 py-2">
    build: {commitHash}
  </footer>
</div>
