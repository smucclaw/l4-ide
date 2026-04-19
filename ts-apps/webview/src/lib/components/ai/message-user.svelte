<script lang="ts">
  import CopyButton from './copy-button.svelte'

  let { content }: { content: string } = $props()
</script>

<div class="user-row">
  <div class="user-bubble">
    <pre class="user-text">{content}</pre>
    <div class="user-copy">
      <CopyButton getText={() => content} />
    </div>
  </div>
</div>

<style>
  .user-row {
    display: flex;
    justify-content: flex-end;
    padding: 4px 0;
  }
  .user-bubble {
    position: relative;
    max-width: 85%;
    background: var(--vscode-input-background, rgba(127, 127, 127, 0.12));
    border: 1px solid var(--vscode-widget-border, rgba(127, 127, 127, 0.2));
    border-radius: 10px;
    padding: 8px 10px;
  }
  .user-text {
    margin: 0;
    white-space: pre-wrap;
    word-break: break-word;
    font-family: inherit;
    font-size: 13px;
    line-height: 1.4;
  }
  .user-copy {
    position: absolute;
    top: 2px;
    right: 2px;
  }
</style>
