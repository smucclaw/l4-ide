<script lang="ts">
  import type { PendingQuestion } from '$lib/stores/ai-chat.svelte'

  let {
    question,
    onAnswer,
  }: {
    question: PendingQuestion
    /** Called with the user's reply. Pass an empty string to signal
     *  "skip — use your best guess", which is what the dispatcher
     *  relays back to the model. */
    onAnswer: (answer: string) => void
  } = $props()

  let text = $state('')

  function submit(): void {
    const trimmed = text.trim()
    if (!trimmed) return
    onAnswer(trimmed)
  }

  function skip(): void {
    onAnswer('')
  }

  function pick(choice: string): void {
    onAnswer(choice)
  }

  function onKeydown(e: KeyboardEvent): void {
    if (e.key === 'Enter' && !e.shiftKey && !e.metaKey && !e.ctrlKey) {
      e.preventDefault()
      submit()
    }
  }
</script>

<div class="ask-card" role="group" aria-label="Model question">
  <div class="ask-question">{question.question}</div>
  {#if question.choices && question.choices.length > 0}
    <div class="ask-label">Choices:</div>
    <div class="ask-choices">
      {#each question.choices as choice (choice)}
        <button class="choice-btn" onclick={() => pick(choice)}>{choice}</button
        >
      {/each}
    </div>
  {/if}
  <div class="ask-label">Custom answer:</div>
  <div class="ask-input-row">
    <input
      class="ask-input"
      type="text"
      bind:value={text}
      onkeydown={onKeydown}
      placeholder="Type your answer…"
    />
    <button
      class="send-btn"
      onclick={submit}
      disabled={!text.trim()}
      title="Send answer (Enter)">Send</button
    >
  </div>
  <div class="ask-skip">
    <button class="skip-btn" onclick={skip}>Skip this and continue</button>
  </div>
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
  .ask-label {
    font-size: 11px;
    text-transform: uppercase;
    letter-spacing: 0.04em;
    margin-top: 4px;
    color: var(--vscode-descriptionForeground);
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
  .ask-input-row {
    display: flex;
    gap: 6px;
  }
  .ask-input {
    flex: 1;
    background: var(--vscode-input-background);
    color: var(--vscode-input-foreground);
    border: 1px solid var(--vscode-widget-border, rgba(128, 128, 128, 0.35));
    border-radius: 4px;
    padding: 6px 8px;
    font-size: 12px;
    outline: none;
  }
  .ask-input:focus {
    border-color: var(--vscode-foreground, #ccc);
  }
  .send-btn {
    background: #c8376a;
    color: #fff;
    border: none;
    padding: 6px 14px;
    border-radius: 4px;
    font-size: 12px;
    cursor: pointer;
  }
  .send-btn:disabled {
    opacity: 0.45;
    cursor: not-allowed;
  }
  .send-btn:hover:not(:disabled) {
    background: #d94d7e;
  }
  .ask-skip {
    display: flex;
    justify-content: flex-end;
  }
  .skip-btn {
    background: transparent;
    border: none;
    color: var(--vscode-descriptionForeground);
    font-size: 11px;
    cursor: pointer;
  }
  .skip-btn:hover {
    color: var(--vscode-foreground);
  }
</style>
