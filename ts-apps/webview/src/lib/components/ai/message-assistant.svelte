<script lang="ts">
  import StreamingMarkdown from './streaming-markdown.svelte'
  import ErrorBubble from './error-bubble.svelte'
  import CopyButton from './copy-button.svelte'

  let {
    content,
    streaming,
    error,
    onRetry,
    onSignIn,
  }: {
    content: string
    streaming: boolean
    error?: { message: string; code?: string }
    onRetry?: () => void
    onSignIn?: () => void
  } = $props()
</script>

<div class="assistant-row">
  <div class="assistant-bubble">
    <StreamingMarkdown text={content} {streaming} />
    {#if !streaming && content}
      <div class="assistant-copy">
        <CopyButton getText={() => content} />
      </div>
    {/if}
    {#if error}
      <ErrorBubble
        message={error.message}
        code={error.code}
        {onRetry}
        {onSignIn}
      />
    {/if}
  </div>
</div>

<style>
  .assistant-row {
    display: flex;
    padding: 4px 0;
  }
  .assistant-bubble {
    position: relative;
    width: 100%;
    font-size: 13px;
    line-height: 1.45;
    color: var(--vscode-foreground);
  }
  .assistant-copy {
    position: absolute;
    top: -4px;
    right: 0;
  }
</style>
