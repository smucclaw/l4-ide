<script lang="ts">
  import type { AiConversationSummary } from 'jl4-client-rpc'
  import { auth, CONSOLE_URL } from '$lib/auth/session.svelte'

  let {
    onNewChat,
    onSelect,
    onDelete,
    items = [],
    currentId = null,
    streamingIds = [],
  }: {
    onNewChat?: () => void
    onSelect?: (id: string) => void
    onDelete?: (id: string) => void
    items?: AiConversationSummary[]
    currentId?: string | null
    streamingIds?: string[]
  } = $props()

  const streamingSet = $derived(new Set(streamingIds))

  let menuOpen = $state(false)

  const user = $derived(auth.user)
  const org = $derived(auth.organization)

  const displayName = $derived(
    [user?.firstName, user?.lastName].filter(Boolean).join(' ') ||
      user?.email ||
      'Account'
  )
  const initials = $derived(
    (user?.firstName?.[0] ?? user?.email?.[0] ?? '?').toUpperCase()
  )

  function closeMenu(): void {
    menuOpen = false
  }
</script>

<svelte:window onclick={closeMenu} />

<aside class="sidebar">
  <div class="top">
    <button class="new-chat" onclick={() => onNewChat?.()}>
      <span class="plus">+</span> New chat
    </button>

    <nav class="history" aria-label="Chat history">
      {#each items as item (item.id)}
        <div class="hist-row" class:active={item.id === currentId}>
          <button
            class="hist-item"
            onclick={() => onSelect?.(item.id)}
            title={item.title}
          >
            {#if streamingSet.has(item.id)}<span class="dot"></span>{/if}
            <span class="hist-title">{item.title}</span>
          </button>
          <button
            class="hist-del"
            title="Delete chat"
            aria-label="Delete chat"
            onclick={(e) => {
              e.stopPropagation()
              onDelete?.(item.id)
            }}
          >
            <svg
              width="16"
              height="16"
              viewBox="0 0 24 24"
              fill="none"
              stroke="currentColor"
              stroke-width="2"
              stroke-linecap="round"
              stroke-linejoin="round"
              aria-hidden="true"
            >
              <polyline points="3 6 5 6 21 6" />
              <path
                d="M19 6v14a2 2 0 0 1-2 2H7a2 2 0 0 1-2-2V6m3 0V4a2 2 0 0 1 2-2h4a2 2 0 0 1 2 2v2"
              />
              <line x1="10" y1="11" x2="10" y2="17" />
              <line x1="14" y1="11" x2="14" y2="17" />
            </svg>
          </button>
        </div>
      {/each}
      {#if items.length === 0}
        <p class="hist-empty">No conversations yet</p>
      {/if}
    </nav>
  </div>

  <div class="bottom">
    <div class="profile-wrap">
      <button
        class="profile"
        onclick={(e) => {
          e.stopPropagation()
          menuOpen = !menuOpen
        }}
        aria-haspopup="menu"
        aria-expanded={menuOpen}
      >
        <span class="avatar">
          {#if user?.profilePictureUrl}
            <img src={user.profilePictureUrl} alt="" />
          {:else}
            {initials}
          {/if}
        </span>
        <span class="who">
          <span class="name">{displayName}</span>
          {#if org}<span class="org">{org.name}</span>{/if}
        </span>
        <svg
          class="chev"
          class:open={menuOpen}
          width="16"
          height="16"
          viewBox="0 0 24 24"
          fill="none"
          stroke="currentColor"
          stroke-width="2"
          stroke-linecap="round"
          stroke-linejoin="round"
          aria-hidden="true"
        >
          <polyline points="6 9 12 15 18 9" />
        </svg>
      </button>

      {#if menuOpen}
        <div class="menu" role="menu">
          <a
            class="menu-item"
            href={CONSOLE_URL}
            target="_blank"
            rel="noopener"
            role="menuitem"
          >
            Legalese Cloud Console
          </a>
          <button
            class="menu-item danger"
            role="menuitem"
            onclick={() => auth.logout()}
          >
            Log out
          </button>
        </div>
      {/if}
    </div>
  </div>
</aside>

<style>
  .sidebar {
    display: flex;
    flex-direction: column;
    height: 100%;
    width: 260px;
    flex: 0 0 260px;
    background: var(--sidebar-bg);
    border-right: 1px solid var(--sidebar-border);
    box-sizing: border-box;
  }

  .top {
    flex: 1 1 auto;
    min-height: 0;
    display: flex;
    flex-direction: column;
    padding: 0.75rem;
    gap: 0.5rem;
  }

  .new-chat {
    display: flex;
    align-items: center;
    gap: 0.4rem;
    width: 100%;
    padding: 0.3rem 0.6rem;
    border-radius: 4px;
    background: var(--chat-bg);
    color: var(--vscode-foreground);
    font-size: 0.85em;
    font-weight: 500;
    cursor: pointer;
    text-align: left;
    transition: background-color 0.1s ease-out;
  }
  .new-chat:hover {
    background: var(--vscode-list-hoverBackground);
  }
  .plus {
    color: var(--brand);
    font-weight: 700;
  }

  .history {
    flex: 1 1 auto;
    min-height: 0;
    overflow-y: auto;
    display: flex;
    flex-direction: column;
    gap: 2px;
  }
  .hist-row {
    display: flex;
    align-items: center;
    border-radius: 6px;
  }
  .hist-row:hover {
    background: var(--vscode-list-hoverBackground);
  }
  .hist-row.active {
    background: var(--vscode-list-activeSelectionBackground);
  }
  .hist-item {
    display: flex;
    align-items: center;
    gap: 0.4rem;
    flex: 1 1 auto;
    min-width: 0;
    padding: 0.4rem 0.5rem;
    border: none;
    border-radius: 4px;
    background: transparent;
    color: var(--vscode-foreground);
    font: inherit;
    font-size: 0.85rem;
    text-align: left;
    cursor: pointer;
  }
  .hist-del {
    flex: 0 0 auto;
    border: none;
    background: transparent;
    color: var(--vscode-descriptionForeground);
    cursor: pointer;
    font-size: 1rem;
    line-height: 1;
    padding: 0.25rem 0.5rem;
    opacity: 0;
  }
  .hist-row:hover .hist-del {
    opacity: 0.8;
  }
  .hist-del:hover {
    color: var(--vscode-errorForeground);
    opacity: 1;
  }
  .hist-title {
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }
  .dot {
    flex: 0 0 auto;
    width: 6px;
    height: 6px;
    border-radius: 50%;
    background: var(--brand);
  }
  .hist-empty {
    color: var(--vscode-descriptionForeground);
    font-size: 0.8rem;
    padding: 0.4rem 0.5rem;
    margin: 0;
  }

  .bottom {
    flex: 0 0 auto;
    border-top: 1px solid var(--sidebar-border);
    padding: 0.5rem;
  }

  .profile-wrap {
    position: relative;
  }

  .profile {
    display: flex;
    align-items: center;
    gap: 0.5rem;
    width: 100%;
    padding: 0.4rem 0.5rem;
    border: none;
    border-radius: 8px;
    background: transparent;
    color: var(--vscode-foreground);
    font: inherit;
    cursor: pointer;
    text-align: left;
  }
  .profile:hover {
    background: var(--vscode-list-hoverBackground);
  }

  .avatar {
    flex: 0 0 auto;
    width: 28px;
    height: 28px;
    border-radius: 50%;
    background: var(--brand);
    color: #fff;
    font-size: 0.8rem;
    font-weight: 600;
    display: grid;
    place-items: center;
    overflow: hidden;
  }
  .avatar img {
    width: 100%;
    height: 100%;
    object-fit: cover;
  }

  .who {
    flex: 1 1 auto;
    min-width: 0;
    display: flex;
    flex-direction: column;
    line-height: 1.2;
  }
  .name {
    font-size: 0.85rem;
    font-weight: 500;
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }
  .org {
    font-size: 0.72rem;
    color: var(--vscode-descriptionForeground);
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }
  .chev {
    flex: 0 0 auto;
    display: block;
    color: var(--vscode-descriptionForeground);
    transition: transform 0.12s ease-out;
  }
  .chev.open {
    transform: rotate(180deg);
  }

  .menu {
    position: absolute;
    bottom: calc(100% + 6px);
    left: 0;
    right: 0;
    background: var(--chat-bg);
    border: 1px solid var(--vscode-widget-border);
    border-radius: 6px;
    box-shadow: 0 6px 24px rgba(0, 0, 0, 0.18);
    padding: 4px;
    z-index: 20;
  }
  .menu-item {
    display: block;
    width: 100%;
    padding: 0.25rem 0.55rem;
    border: none;
    border-radius: 4px;
    background: transparent;
    color: var(--vscode-foreground);
    font: inherit;
    font-size: 0.85rem;
    text-align: left;
    text-decoration: none;
    cursor: pointer;
    box-sizing: border-box;
  }
  .menu-item:hover {
    background: var(--vscode-list-hoverBackground);
  }
  .menu-item.danger {
    color: var(--vscode-errorForeground);
  }
</style>
