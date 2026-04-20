<script lang="ts">
  import StreamingMarkdown from './streaming-markdown.svelte'
  import ErrorBubble from './error-bubble.svelte'
  import CopyButton from './copy-button.svelte'
  import ToolCallRow from './tool-call-row.svelte'
  import type { AssistantBlock } from '$lib/stores/ai-chat.svelte'

  let {
    content,
    streaming,
    error,
    blocks,
    onRetry,
    onSignIn,
    onOpenFile,
    onOpenFileDiff,
  }: {
    content: string
    streaming: boolean
    error?: { message: string; code?: string }
    blocks?: AssistantBlock[]
    onRetry?: () => void
    onSignIn?: () => void
    onOpenFile: (callId: string) => void
    onOpenFileDiff: (callId: string) => void
  } = $props()

  // Stable keys for the block iteration. Text blocks don't carry an
  // id, so we use the index; tool-call blocks key by callId.
  function blockKey(b: AssistantBlock, i: number): string {
    return b.kind === 'tool-call' ? `tc:${b.call.callId}` : `tx:${i}`
  }
</script>

<div class="assistant-row">
  <div class="assistant-bubble">
    {#if blocks && blocks.length > 0}
      {#each blocks as block, i (blockKey(block, i))}
        {#if block.kind === 'text'}
          <StreamingMarkdown
            text={block.text}
            streaming={streaming && i === blocks.length - 1}
          />
        {:else}
          <ToolCallRow
            call={block.call}
            {onOpenFile}
            onOpenDiff={onOpenFileDiff}
          />
        {/if}
      {/each}
    {:else if content}
      <StreamingMarkdown text={content} {streaming} />
    {/if}
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
