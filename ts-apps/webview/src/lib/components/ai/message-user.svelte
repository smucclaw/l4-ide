<script lang="ts">
  // import CopyButton from './copy-button.svelte'

  let {
    content,
    shouldStick = false,
    userIndex = -1,
  }: {
    content: string
    shouldStick?: boolean
    userIndex?: number
  } = $props()
</script>

<div
  class="user-row user-message-wrapper"
  class:sticky={shouldStick}
  data-user-index={userIndex}
>
  <div class="user-bubble">
    <pre class="user-text">{content}</pre>
    <!-- <div class="user-copy">
      <CopyButton getText={() => content} />
    </div> -->
  </div>
</div>

<style>
  .user-row {
    display: flex;
    padding: 16px 0 4px;
  }
  .user-row:first-child {
    padding-top: 0px;
  }
  .user-row.sticky {
    position: sticky;
    padding-top: 0px;
    top: 0px;
    z-index: 10;
  }
  .user-bubble {
    position: relative;
    flex: 1;
    background: var(--vscode-input-background, rgba(127, 127, 127, 0.12));
    border: 1px solid var(--vscode-widget-border, rgba(127, 127, 127, 0.2));
    border-radius: 10px;
    padding: 8px 10px;
    margin-left: -10px;
    margin-right: -10px;
    box-shadow: 0 0 16px 2px
      var(--vscode-input-background, rgba(127, 127, 127, 0.12));
  }
  .user-text {
    margin: 0;
    white-space: pre-wrap;
    word-break: break-word;
    font-family: inherit;
    font-size: 13px;
    line-height: 1.4;
  }
  /* .user-copy {
    position: absolute;
    top: 2px;
    right: 2px;
  } */
</style>
