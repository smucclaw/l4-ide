<script lang="ts">
  import { tick } from 'svelte'
  import UserMessage from './message-user.svelte'
  import AssistantMessage from './message-assistant.svelte'
  import type { RenderedTurn } from '$lib/stores/ai-chat.svelte'

  let {
    turns,
    onRetry,
    onSignIn,
    onApproveTool,
    onOpenFileDiff,
  }: {
    turns: RenderedTurn[]
    onRetry?: () => void
    onSignIn?: () => void
    onApproveTool: (
      callId: string,
      decision: 'allow' | 'deny' | 'alwaysAllow'
    ) => void
    onOpenFileDiff: (callId: string) => void
  } = $props()

  let scrollEl = $state<HTMLDivElement>()
  let stickToBottom = $state(true)
  let showJumpButton = $state(false)

  function onScroll(): void {
    if (!scrollEl) return
    const near =
      scrollEl.scrollHeight - scrollEl.scrollTop - scrollEl.clientHeight < 40
    stickToBottom = near
    showJumpButton = !near
  }

  async function jumpToLatest(): Promise<void> {
    await tick()
    if (!scrollEl) return
    scrollEl.scrollTop = scrollEl.scrollHeight
    stickToBottom = true
    showJumpButton = false
  }

  // When new tokens land, auto-scroll only if the user is near the
  // bottom. Otherwise leave them where they are and surface the
  // "Jump to latest" affordance.
  $effect(() => {
    // Track changes by reading every turn's content length.
    // eslint-disable-next-line @typescript-eslint/no-unused-expressions
    turns.map((t) => t.content.length)
    if (stickToBottom && scrollEl) {
      // Microtask so the DOM has grown before we measure.
      queueMicrotask(() => {
        if (scrollEl && stickToBottom) {
          scrollEl.scrollTop = scrollEl.scrollHeight
        }
      })
    }
  })
</script>

<div class="message-list" bind:this={scrollEl} onscroll={onScroll}>
  {#each turns as turn (turn.id)}
    {#if turn.role === 'user'}
      <UserMessage content={turn.content} />
    {:else}
      <AssistantMessage
        content={turn.content}
        streaming={!!turn.streaming}
        error={turn.error}
        toolCalls={turn.toolCalls}
        {onRetry}
        {onSignIn}
        {onApproveTool}
        {onOpenFileDiff}
      />
    {/if}
  {/each}
  {#if showJumpButton}
    <button class="jump-btn" onclick={jumpToLatest} title="Jump to latest">
      ↓ Latest
    </button>
  {/if}
</div>

<style>
  .message-list {
    position: relative;
    flex: 1;
    min-height: 0;
    overflow-y: auto;
    overflow-x: hidden;
    padding: 8px 10px;
    display: flex;
    flex-direction: column;
    gap: 2px;
  }
  .jump-btn {
    position: sticky;
    bottom: 8px;
    align-self: center;
    background: var(--vscode-sideBar-background, rgba(128, 128, 128, 0.3));
    color: var(--vscode-button-secondaryForeground, inherit);
    border: 1px solid var(--vscode-widget-border, rgba(128, 128, 128, 0.35));
    border-radius: 12px;
    padding: 3px 10px;
    font-size: 11px;
    cursor: pointer;
    margin-top: 8px;
  }
</style>
