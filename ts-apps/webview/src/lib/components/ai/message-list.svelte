<script lang="ts">
  import { tick } from 'svelte'
  import UserMessage from './message-user.svelte'
  import AssistantMessage from './message-assistant.svelte'
  import SectionSpinner from './section-spinner.svelte'
  import AskUserCard from './ask-user-card.svelte'
  import type {
    RenderedTurn,
    RenderedToolCall,
    PendingQuestion,
  } from '$lib/stores/ai-chat.svelte'

  let {
    turns,
    streaming,
    pendingApproval,
    pendingQuestion,
    onRetry,
    onApproveTool,
    onAnswerQuestion,
    onOpenFile,
    onOpenFileDiff,
  }: {
    turns: RenderedTurn[]
    /** True while the current conversation has an open stream. Drives
     *  the bottom-of-chat § spinner. */
    streaming: boolean
    /** First tool call awaiting approval, if any. When present, the
     *  bottom action bar replaces the spinner with Accept / Reject. */
    pendingApproval: RenderedToolCall | null
    /** Active meta__ask_user question awaiting an answer, or null. */
    pendingQuestion: PendingQuestion | null
    onRetry?: () => void
    onApproveTool: (
      callId: string,
      decision: 'allow' | 'deny' | 'alwaysAllow'
    ) => void
    onAnswerQuestion: (answer: string) => void
    onOpenFile: (callId: string) => void
    onOpenFileDiff: (callId: string) => void
  } = $props()

  let scrollEl = $state<HTMLDivElement>()
  let stickToBottom = $state(true)
  let showJumpButton = $state(false)
  let stickyUserIndex = $state<number>(-1)
  let userMessageOffsets: { index: number; offsetTop: number }[] = []

  function onScroll(): void {
    if (!scrollEl) return
    const near =
      scrollEl.scrollHeight - scrollEl.scrollTop - scrollEl.clientHeight < 40
    stickToBottom = near
    showJumpButton = !near

    // Find the last user message whose offsetTop is less than scrollTop
    const scrollTop = scrollEl.scrollTop
    let lastUserAboveScroll = -1

    for (const { index, offsetTop } of userMessageOffsets) {
      if (offsetTop < scrollTop) {
        lastUserAboveScroll = index
      } else {
        break
      }
    }

    stickyUserIndex = lastUserAboveScroll
  }

  // Update offsets when turns change
  $effect(() => {
    if (!scrollEl) return
    // Trigger on turns change
    turns.length

    // Wait for DOM to update
    queueMicrotask(() => {
      if (!scrollEl) return
      const offsets: { index: number; offsetTop: number }[] = []
      const children = scrollEl.children

      for (let i = 0; i < children.length; i++) {
        const child = children[i] as HTMLElement
        if (!child.classList.contains('user-message-wrapper')) continue

        const userIndex = parseInt(child.dataset.userIndex || '-1', 10)
        if (userIndex >= 0) {
          offsets.push({ index: userIndex, offsetTop: child.offsetTop })
        }
      }

      userMessageOffsets = offsets
    })
  })

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
  {#each turns as turn, i (turn.id)}
    {#if turn.role === 'user'}
      {@const userIndex =
        turns.slice(0, i + 1).filter((t) => t.role === 'user').length - 1}
      <UserMessage
        content={turn.content}
        chips={turn.chips}
        shouldStick={stickyUserIndex === userIndex}
        {userIndex}
      />
    {:else}
      <AssistantMessage
        content={turn.content}
        streaming={!!turn.streaming}
        error={turn.error}
        blocks={turn.blocks}
        usage={turn.usage}
        {onRetry}
        {onOpenFile}
        {onOpenFileDiff}
      />
    {/if}
  {/each}
  {#if showJumpButton && !pendingQuestion}
    <!-- Hide the sticky scroll-assist while a meta__ask_user card is
         active: the question card is the only thing the user can act
         on, and the sticky button would otherwise float above it at
         the bottom of the viewport (its container is the whole scroll
         region, so it keeps painting regardless of the card's
         position) and draw the eye away from the required action. -->
    <button class="jump-btn" onclick={jumpToLatest} title="Jump to latest">
      ↓ Latest
    </button>
  {/if}

  <!-- Question card for an active meta__ask_user. Rendered above the
       spinner / approval bar so the user can't miss it. Dispatcher is
       blocked on their reply, so this is the only meaningful action
       they can take. -->
  {#if pendingQuestion}
    <AskUserCard question={pendingQuestion} onAnswer={onAnswerQuestion} />
  {/if}

  <!-- Bottom-of-chat status. Shows Accept/Reject for any pending
       tool-call approval; otherwise the § spinner while the stream
       is live; otherwise nothing. -->
  {#if pendingApproval}
    <div class="bottom-approve" role="group" aria-label="Approve tool call">
      <div class="bottom-approve-label">
        Allow <strong>{pendingApproval.name}</strong>?
      </div>
      <div class="bottom-approve-btns">
        <button
          class="approve-btn allow"
          onclick={() => onApproveTool(pendingApproval!.callId, 'allow')}
          >Accept</button
        >
        <button
          class="approve-btn always"
          title="Always allow this category of tool"
          onclick={() => onApproveTool(pendingApproval!.callId, 'alwaysAllow')}
          >Always accept</button
        >
        <button
          class="approve-btn deny"
          onclick={() => onApproveTool(pendingApproval!.callId, 'deny')}
          >Reject</button
        >
      </div>
    </div>
  {:else if streaming && !pendingQuestion}
    <!-- Hide the section-sign spinner while a meta__ask_user is
         pending: the turn is technically still open but the stream
         is paused awaiting the user, so showing the running indicator
         would falsely imply the model is doing work. The question
         card above already signals the state. -->
    <div class="bottom-spinner">
      <SectionSpinner size={48} />
    </div>
  {/if}
</div>

<style>
  .message-list {
    position: relative;
    flex: 1;
    min-height: 0;
    overflow-y: auto;
    overflow-x: hidden;
    padding: 0 10px 16px;
    display: block;
  }
  .jump-btn {
    position: sticky;
    bottom: 4px;
    background: var(--vscode-sideBar-background, rgba(128, 128, 128, 0.3));
    color: var(--vscode-button-secondaryForeground, inherit);
    border: 1px solid var(--vscode-widget-border, rgba(128, 128, 128, 0.35));
    border-radius: 12px;
    padding: 3px 10px;
    font-size: 11px;
    cursor: pointer;
    margin-top: 8px;
    margin-right: -10px;
  }
  .bottom-spinner {
    display: flex;
    justify-content: center;
    align-items: center;
    padding: 4px 0 16px;
  }
  .bottom-approve {
    display: flex;
    flex-direction: column;
    align-items: center;
    gap: 6px;
    padding: 10px 0 16px;
  }
  .bottom-approve-label {
    font-size: 11px;
    color: var(--vscode-descriptionForeground);
    text-align: center;
  }
  .bottom-approve-label strong {
    color: var(--vscode-foreground);
    font-family: var(--vscode-editor-font-family, monospace);
    font-weight: normal;
  }
  .bottom-approve-btns {
    display: flex;
    gap: 6px;
  }
  .approve-btn {
    border: 1px solid var(--vscode-widget-border, rgba(128, 128, 128, 0.35));
    background: transparent;
    color: var(--vscode-foreground);
    padding: 3px 14px;
    font-size: 12px;
    border-radius: 3px;
    cursor: pointer;
  }
  .approve-btn:hover {
    border-color: var(--vscode-foreground);
  }
  .approve-btn.allow {
    background: #c8376a;
    border-color: transparent;
    color: #fff;
  }
  .approve-btn.allow:hover {
    background: #d94d7e;
    border-color: transparent;
  }
</style>
