<script lang="ts">
  import { Separator, Toolbar, Tooltip } from "bits-ui"
  import Fa from 'svelte-fa'
  import { faDiagramProject } from '@fortawesome/free-solid-svg-icons'
  import { faVolumeXmark } from '@fortawesome/free-solid-svg-icons'
  import { faPersonCircleQuestion } from '@fortawesome/free-solid-svg-icons'
 
  let text = $state(["bold"])
  let align = $state("")
  export { Toolbar }
</script>
 
<Toolbar.Root
  class="rounded-10px border-gray-700 shadow-mini flex h-12 max-w-max items-center justify-center border px-[4px] py-1"
>
  <Toolbar.Group
    bind:value={text}
    type="multiple"
    class="flex items-center gap-x-0.5"
  >
    <Toolbar.GroupItem
      aria-label="toggle italic"
      value="italic"
      class="rounded-9px text-foreground/60 data-[state=on]:text-foreground/80 inline-flex size-10 items-center justify-center transition-all active:scale-[0.98]"
    >
      <Tooltip.Provider>
        <Tooltip.Root>
          <Tooltip.Trigger
            class="shadow-btn hover:bg-muted background focus-visible:outline-hidden inline-flex size-10 items-center justify-center"
          >
            <Fa icon={faDiagramProject} class="size-6" />
          </Tooltip.Trigger>
          <Tooltip.Content side="bottom">
            <div
              class="rounded-input border-dark-10 bg-gray-500 text-white shadow-popover outline-hidden flex items-center justify-center border p-1 text-sm font-medium"
            >
              Select paths
            </div>
          </Tooltip.Content>
        </Tooltip.Root>
      </Tooltip.Provider>
    </Toolbar.GroupItem>
    <Toolbar.GroupItem
      aria-label="toggle strikethrough"
      value="strikethrough"
      class="rounded-9px text-foreground/60 data-[state=on]:text-foreground/80 active:data-[state=on]inline-flex size-10 items-center justify-center transition-all active:scale-[0.98]"
    >
      <Tooltip.Provider>
        <Tooltip.Root>
          <Tooltip.Trigger
            class="shadow-btn hover:bg-muted background focus-visible:outline-hidden inline-flex size-10 items-center justify-center"
          >
            <Fa icon={faPersonCircleQuestion} class="size-6" />
          </Tooltip.Trigger>
          <Tooltip.Content side="bottom">
            <div
              class="rounded-input border-gray-700 bg-gray-500 text-white shadow-popover outline-hidden flex items-center justify-center border p-1 text-sm font-medium"
            >
              #WhatIf
            </div>
          </Tooltip.Content>
        </Tooltip.Root>
      </Tooltip.Provider>
    </Toolbar.GroupItem>
  </Toolbar.Group>
 
  <Separator.Root class="bg-gray-700 -my-1 mx-1 w-[1px] self-stretch" />
 
  <Toolbar.Group
    bind:value={align}
    type="single"
    class="flex items-center gap-x-0.5"
  >
    <Toolbar.GroupItem
      aria-label="align left"
      value="left"
      class="rounded-9px bg-background-alt text-foreground/60 hover:bg-muted active:bg-dark-10 data-[state=on]:bg-muted data-[state=on]:text-foreground/80 active:data-[state=on]:bg-dark-10 inline-flex size-10 items-center justify-center transition-all active:scale-[0.98]"
    >
    <Tooltip.Provider>
      <Tooltip.Root>
        <Tooltip.Trigger
          class="shadow-btn hover:bg-muted background focus-visible:outline-hidden inline-flex size-10 items-center justify-center"
        >
          <Fa icon={faVolumeXmark} class="size-6" />
          </Tooltip.Trigger>
          <Tooltip.Content side="bottom">
            <div
              class="rounded-input border-dark-10 bg-gray-500 text-white shadow-popover outline-hidden flex items-center justify-center border p-1 text-sm font-medium"
            >
              Zen mode
            </div>
          </Tooltip.Content>
        </Tooltip.Root>
      </Tooltip.Provider>
    </Toolbar.GroupItem>
  </Toolbar.Group>
</Toolbar.Root>