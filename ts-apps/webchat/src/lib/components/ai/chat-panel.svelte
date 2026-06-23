<script lang="ts">
  import type { Messenger } from 'vscode-messenger-webview'
  import type { AiChatStore } from '$lib/stores/ai-chat.svelte'
  import MessageList from './message-list.svelte'
  import EmptyState from './empty-state.svelte'

  let {
    store,
    messenger,
  }: {
    store: AiChatStore
    messenger: InstanceType<typeof Messenger> | null
  } = $props()

  const showEmptyState = $derived(!store.current)

  function onRetry(): void {
    store.continueTurn()
  }
</script>

<div class="conversation">
  {#if showEmptyState}
    <EmptyState
      deployment={store.deploymentBinding
        ? {
            deploymentId: store.deploymentBinding.deploymentId,
            intendedUse: store.deploymentBinding.intendedUse ?? '',
          }
        : null}
    />
  {:else if store.current}
    <MessageList
      turns={store.current.turns}
      streaming={store.current.streaming}
      pipelineActive={store.pipelineActive}
      pendingApproval={store.pendingApproval}
      pendingQuestion={store.pendingQuestion}
      {messenger}
      {onRetry}
      onApproveTool={(callId, decision) => store.approveTool(callId, decision)}
      onAnswerQuestion={(answer) => store.answerQuestion(answer)}
      onOpenFile={(callId) => store.openFile(callId)}
      onOpenFileDiff={(callId) => store.openFileDiff(callId)}
    />
  {/if}
</div>

<style>
  .conversation {
    flex: 1 1 auto;
    max-width: 800px;
    width: 100%;
    min-height: 0;
    display: flex;
    flex-direction: column;
    overflow: hidden;
    padding-top: 8px;
  }
</style>
