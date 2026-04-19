<script lang="ts">
  import StreamingMarkdown from './streaming-markdown.svelte'
  import ErrorBubble from './error-bubble.svelte'
  import CopyButton from './copy-button.svelte'
  import SectionSpinner from './section-spinner.svelte'
  import ToolCallRow from './tool-call-row.svelte'
  import type { RenderedToolCall } from '$lib/stores/ai-chat.svelte'

  let {
    content,
    streaming,
    error,
    toolCalls,
    onRetry,
    onSignIn,
    onApproveTool,
    onOpenFileDiff,
  }: {
    content: string
    streaming: boolean
    error?: { message: string; code?: string }
    toolCalls?: RenderedToolCall[]
    onRetry?: () => void
    onSignIn?: () => void
    onApproveTool: (
      callId: string,
      decision: 'allow' | 'deny' | 'alwaysAllow'
    ) => void
    onOpenFileDiff: (callId: string) => void
  } = $props()

  // Spinner only while we're pre-first-token with no tool activity.
  // Once either text or a tool row lands, swap to the real content.
  const showSpinner = $derived(
    streaming && !content && !error && (!toolCalls || toolCalls.length === 0)
  )
</script>

<div class="assistant-row">
  <div class="assistant-bubble">
    {#if showSpinner}
      <div class="waiting">
        <SectionSpinner size={52} />
      </div>
    {:else}
      {#if content}
        <StreamingMarkdown text={content} {streaming} />
      {/if}
      {#if toolCalls && toolCalls.length > 0}
        <div class="tool-calls">
          {#each toolCalls as call (call.callId)}
            <ToolCallRow
              {call}
              onApprove={onApproveTool}
              onOpenDiff={onOpenFileDiff}
            />
          {/each}
        </div>
      {/if}
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
  .waiting {
    display: flex;
    align-items: center;
    padding: 6px 0 8px;
  }
  .tool-calls {
    display: flex;
    flex-direction: column;
    gap: 2px;
    padding: 4px 0 2px;
  }
</style>
