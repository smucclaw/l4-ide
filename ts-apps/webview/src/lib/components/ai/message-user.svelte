<script lang="ts">
  // import CopyButton from './copy-button.svelte'
  import type { UserTurnChip } from '$lib/stores/ai-chat.svelte'

  let {
    content,
    chips,
    shouldStick = false,
    userIndex = -1,
    pending = false,
  }: {
    content: string
    chips?: UserTurnChip[]
    shouldStick?: boolean
    userIndex?: number
    /** True for a user bubble that's still queued in the extension's
     *  inject pipeline (the extension has not yet echoed
     *  `queue-consumed` for it). Renders the bubble at reduced
     *  opacity so the user can see it landed but isn't part of the
     *  active model request yet, and excludes it from the
     *  scroll-sticky header set (the data-pending attribute below
     *  is read by message-list's offset collector). */
    pending?: boolean
  } = $props()
</script>

<div
  class="user-row user-message-wrapper"
  class:sticky={shouldStick}
  class:pending
  data-user-index={userIndex}
  data-pending={pending ? 'true' : 'false'}
>
  <div class="user-bubble">
    {#if chips && chips.length > 0}
      <!-- Echo the staged-context chips captured at submit time. Same
           mild gray fill / layout as the input strip so the bubble
           reads as "this is what I sent you". -->
      <div class="user-chips" role="list" aria-label="Attached context">
        {#each chips as chip, i (i)}
          <span
            class="user-chip"
            role="listitem"
            title={chip.kind === 'active-file' ? chip.path : chip.name}
          >
            <svg
              class="chip-icon"
              viewBox="0 0 16 16"
              aria-hidden="true"
              fill="none"
              stroke="currentColor"
              stroke-width="1.4"
              stroke-linejoin="round"
            >
              {#if chip.kind === 'image'}
                <rect x="2" y="3" width="12" height="10" rx="1.5" />
                <circle cx="6" cy="7" r="1.2" />
                <path d="M14 11l-3.5-3.5L4 13" />
              {:else}
                <!-- Document glyph covers both `pdf` attachments and
                     the `active-file` context chip. -->
                <path d="M4 2h5l3 3v9H4z" />
                <path d="M9 2v3h3" />
              {/if}
            </svg>
            <span class="chip-name">{chip.name}</span>
          </span>
        {/each}
      </div>
    {/if}
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
  /* Pending injection — the extension hasn't yet echoed
     consumption of this submit. Reads as "queued" rather than
     "sent": dim the bubble + chips so the user can see it landed
     but understand it isn't part of the active model request yet.
     Snaps back to full opacity once `onQueueConsumed` clears the
     pending flag in the store. */
  .user-row.pending .user-bubble {
    opacity: 0.55;
    transition: opacity 0.15s ease-out;
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
  /* Chips echoed above the user's text. Same mild-gray fill as the
     input-box staged chips so visually they feel like the same object
     that just got sent. No remove / preview affordances here — this
     is a read-only record of what went along with the message. */
  .user-chips {
    display: flex;
    flex-wrap: wrap;
    gap: 4px;
    margin-bottom: 6px;
  }
  .user-chip {
    display: inline-flex;
    align-items: center;
    gap: 4px;
    padding: 2px 8px 2px 6px;
    background: rgba(128, 128, 128, 0.14);
    border-radius: 3px;
    font-size: 11px;
    color: var(--vscode-descriptionForeground);
    max-width: 220px;
  }
  .chip-icon {
    width: 12px;
    height: 12px;
    flex-shrink: 0;
  }
  .chip-name {
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }
  /* When the user bubble is stuck to the top of the scroll area, cap
     it at ~3 lines so a long prompt doesn't occlude the assistant
     stream underneath. The fade-mask hints that more text is hidden;
     the unpinned copy below (the full-length original in the flow)
     still renders unclipped. */
  .user-row.sticky .user-text {
    max-height: calc(1.4em * 3);
    overflow: hidden;
    mask-image: linear-gradient(
      to bottom,
      black 0,
      black calc(1.4em * 2.25),
      transparent 100%
    );
    -webkit-mask-image: linear-gradient(
      to bottom,
      black 0,
      black calc(1.4em * 2.25),
      transparent 100%
    );
  }
  /* .user-copy {
    position: absolute;
    top: 2px;
    right: 2px;
  } */
</style>
