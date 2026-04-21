<script lang="ts">
  import type { PendingQuestion } from '$lib/stores/ai-chat.svelte'

  let {
    question,
    onAnswer,
  }: {
    question: PendingQuestion
    /** Called when the user clicks one of the fixed choices. Free-text
     *  answers now come from the main chat input below (see
     *  chat-input.svelte — its submit handler routes to
     *  `store.answerQuestion` whenever there's a pending question). */
    onAnswer: (answer: string) => void
  } = $props()

  function pick(choice: string): void {
    onAnswer(choice)
  }
</script>

<div class="ask-card" role="group" aria-label="Model question">
  <div class="ask-question">{question.question}</div>
  {#if question.choices && question.choices.length > 0}
    <div class="ask-choices">
      {#each question.choices as choice (choice)}
        <button class="choice-btn" onclick={() => pick(choice)}>{choice}</button
        >
      {/each}
    </div>
  {/if}
</div>

<style>
  .ask-card {
    display: flex;
    flex-direction: column;
    gap: 6px;
    padding: 10px 12px;
    margin: 0;
    border: 1px solid var(--vscode-widget-border, rgba(128, 128, 128, 0.35));
    border-radius: 6px;
    background: var(--vscode-editor-background);
  }
  .ask-question {
    font-size: 13px;
    color: var(--vscode-foreground);
    line-height: 1.5em;
    white-space: pre-wrap;
  }
  .ask-choices {
    display: flex;
    flex-wrap: wrap;
    gap: 6px;
    margin-top: 2px;
  }
  .choice-btn {
    border: 1px solid var(--vscode-widget-border, rgba(128, 128, 128, 0.35));
    background: transparent;
    color: var(--vscode-foreground);
    padding: 4px 10px;
    border-radius: 4px;
    font-size: 12px;
    cursor: pointer;
  }
  .choice-btn:hover {
    border-color: var(--vscode-foreground);
  }
</style>
