<script lang="ts">
  // A button that opens a dropdown of local AI harnesses to install the
  // L4 Rules into. Reused in two places: the deployments info box ("Install
  // Skills Marketplace" — the global gateway) and the Integrate popover
  // ("Install as Skill" — a single deployment). The parent decides what each
  // choice does by wiring `onChoose`.
  import { HARNESSES, type Harness } from 'jl4-client-rpc'

  let {
    label,
    title = '',
    up = false,
    extra = [],
    onChoose,
  }: {
    label: string
    title?: string
    /** Open the menu upward (for buttons near the bottom of the panel). */
    up?: boolean
    /** Extra choices appended after a separator, e.g. download-zip. */
    extra?: ReadonlyArray<{ id: string; label: string }>
    onChoose: (id: Harness | string) => void
  } = $props()

  let open = $state(false)
  let root = $state<HTMLDivElement | undefined>(undefined)

  function choose(id: Harness | string): void {
    open = false
    onChoose(id)
  }

  // Close when clicking anywhere outside the menu.
  function onWindowPointerDown(e: PointerEvent): void {
    if (open && root && !root.contains(e.target as Node)) open = false
  }
</script>

<svelte:window onpointerdown={onWindowPointerDown} />

<div class="hm-wrapper" bind:this={root}>
  <button
    class="hm-button"
    {title}
    aria-haspopup="menu"
    aria-expanded={open}
    onclick={() => (open = !open)}
  >
    <span>{label}</span>
  </button>
  {#if open}
    <div class="hm-menu {up ? 'up' : 'down'}" role="menu">
      {#each HARNESSES as h (h.id)}
        <button class="hm-item" role="menuitem" onclick={() => choose(h.id)}
          >{h.label}</button
        >
      {/each}
      {#if extra.length}
        <div class="hm-sep"></div>
        {#each extra as x (x.id)}
          <button class="hm-item" role="menuitem" onclick={() => choose(x.id)}
            >{x.label}</button
          >
        {/each}
      {/if}
    </div>
  {/if}
</div>

<style>
  .hm-wrapper {
    position: relative;
    display: inline-block;
  }

  .hm-button {
    display: inline-flex;
    align-items: center;
    gap: 6px;
    /* Vertical metrics (padding + font-size) match the Integrate
       popover's .value-input / .copy-btn so the Install button sits at
       the same height as the copy buttons in that row. */
    padding: 7px 14px;
    background: var(--vscode-button-secondaryBackground, transparent);
    color: var(--vscode-button-secondaryForeground, var(--vscode-foreground));
    border: 1px solid var(--vscode-widget-border, rgba(128, 128, 128, 0.35));
    border-radius: 4px;
    cursor: pointer;
    font-size: 0.9em;
  }
  .hm-button:hover {
    background: var(
      --vscode-button-secondaryHoverBackground,
      var(--vscode-list-hoverBackground)
    );
  }
  /* Chrome matches the connection-status dropdown (.dropdown-menu in
     the sidebar) so both menus read as the same control. */
  .hm-menu {
    position: absolute;
    left: 0;
    min-width: 100%;
    z-index: 1100;
    display: flex;
    flex-direction: column;
    padding: 4px 0;
    background: var(--vscode-menu-background, #252526);
    border: 1px solid var(--vscode-menu-border, #454545);
    border-radius: 4px;
    box-shadow: 0 2px 8px rgba(0, 0, 0, 0.3);
    white-space: nowrap;
  }
  .hm-menu.down {
    top: calc(100% + 4px);
  }
  .hm-menu.up {
    bottom: calc(100% + 4px);
  }

  .hm-item {
    display: block;
    width: 100%;
    box-sizing: border-box;
    text-align: left;
    padding: 5px 12px;
    background: none;
    border: none;
    color: var(--vscode-menu-foreground, #ccc);
    cursor: pointer;
    font-size: 0.92em;
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
  }
  .hm-item:hover {
    background: var(--vscode-menu-selectionBackground, #04395e);
    color: var(--vscode-menu-selectionForeground, #fff);
  }

  .hm-sep {
    height: 1px;
    margin: 4px 0;
    background: var(--vscode-menu-separatorBackground, #454545);
  }
</style>
