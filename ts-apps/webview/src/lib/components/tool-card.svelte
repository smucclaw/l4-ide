<script lang="ts">
  import type { ExportedFunctionInfo, FunctionParameter } from 'jl4-client-rpc'

  let {
    func,
    initialExpanded = false,
  }: { func: ExportedFunctionInfo; initialExpanded?: boolean } = $props()

  let expanded = $state(initialExpanded)

  function toggle() {
    expanded = !expanded
  }

  function paramEntries(
    params: ExportedFunctionInfo['parameters']
  ): [string, FunctionParameter][] {
    const order = params.required ?? Object.keys(params.properties ?? {})
    const allKeys = new Set([...order, ...Object.keys(params.properties ?? {})])
    return Array.from(allKeys)
      .filter((k) => params.properties?.[k] != null)
      .map((k) => [k, params.properties[k]])
  }

  let paramCount = $derived(
    Object.keys(func.parameters?.properties ?? {}).length
  )

  function isRequired(
    name: string,
    params: ExportedFunctionInfo['parameters']
  ): boolean {
    return params.required?.includes(name) ?? false
  }

  function hasNestedProps(param: FunctionParameter): boolean {
    return (
      param.properties !== undefined && Object.keys(param.properties).length > 0
    )
  }
</script>

<div class="tool-card" class:expanded>
  <button class="card-header" onclick={toggle}>
    <span class="chevron" class:rotated={expanded}>&#9002;</span>
    <span class="func-name">{func.name}</span>
    {#if func.isDefault}
      <span class="badge default-badge">DEFAULT</span>
    {/if}
    {#if !expanded && paramCount > 0}
      <span class="param-count"
        >{paramCount} param{paramCount !== 1 ? 's' : ''}</span
      >
    {/if}
    {#if func.returnType}
      <span class="return-type">{func.returnType}</span>
    {/if}
  </button>

  {#if func.description}
    <div class="func-description">{func.description}</div>
  {/if}

  {#if expanded}
    <div class="card-body">
      {#if paramCount > 0}
        <div class="section-label">Inputs</div>
        <div class="params-list">
          {#each paramEntries(func.parameters) as [name, param]}
            <div class="param-row">
              <span class="param-name">{name}</span>
              <span class="param-type"
                >{param.type}{#if param.alias}
                  ({param.alias}){/if}</span
              >
              {#if isRequired(name, func.parameters)}
                <span class="badge required-badge">required</span>
              {/if}
            </div>
            {#if param.description}
              <div class="param-desc">{param.description}</div>
            {/if}
            {#if param.enum && param.enum.length > 0}
              <div class="param-enums">
                {#each param.enum as val}
                  <span class="enum-tag">{val}</span>
                {/each}
              </div>
            {/if}
            {#if hasNestedProps(param)}
              <div class="nested-props">
                {#each Object.entries(param.properties ?? {}) as [nestedName, nestedParam]}
                  <div class="param-row nested">
                    <span class="param-name">{nestedName}</span>
                    <span class="param-type">{nestedParam.type}</span>
                  </div>
                  {#if nestedParam.description}
                    <div class="param-desc nested">
                      {nestedParam.description}
                    </div>
                  {/if}
                {/each}
              </div>
            {/if}
          {/each}
        </div>
      {/if}

      {#if func.returnType}
        <div class="returns-separator"></div>
        <div class="section-label">Returns</div>
        <div class="return-info">
          <span class="param-type">{func.returnType}</span>
          {#if func.isDeontic}
            <span class="badge deontic-badge">deontic</span>
          {/if}
        </div>
      {/if}
    </div>
  {/if}
</div>

<style>
  .tool-card {
    border: 1px solid var(--vscode-panel-border, #444);
    border-radius: 4px;
    margin-bottom: 6px;
    overflow: hidden;
  }

  .card-header {
    display: flex;
    align-items: center;
    gap: 6px;
    padding: 5px 8px;
    background: var(--vscode-sideBarSectionHeader-background, #252526);
    border: none;
    color: var(--vscode-foreground);
    cursor: pointer;
    width: 100%;
    text-align: left;
    font-family: inherit;
    font-size: inherit;
  }

  .card-header:hover {
    background: var(--vscode-list-hoverBackground, #2a2d2e);
  }

  .chevron {
    display: inline-block;
    color: var(--vscode-descriptionForeground);
    transition: transform 0.15s;
    transform-origin: 25% 50%;
    font-size: 11px;
    flex-shrink: 0;
  }

  .chevron.rotated {
    transform: rotate(90deg);
  }

  .func-name {
    font-family: var(--vscode-editor-font-family, monospace);
    font-size: 0.95em;
    flex: 1;
    min-width: 0;
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }

  .param-count {
    font-size: 0.82em;
    color: var(--vscode-descriptionForeground);
    opacity: 0.6;
    flex-shrink: 0;
    margin-left: auto;
  }

  .return-type {
    font-family: var(--vscode-editor-font-family, monospace);
    font-size: 0.88em;
    color: var(--l4-tok-identifier, #4ec9b0);
    flex-shrink: 0;
    margin-left: auto;
  }

  .param-count + .return-type {
    margin-left: 6px;
  }

  .func-description {
    padding: 2px 8px 5px 25px;
    font-size: 0.88em;
    color: var(--vscode-descriptionForeground);
    background: var(--vscode-sideBarSectionHeader-background, #252526);
  }

  .card-body {
    padding: 6px 8px 8px 8px;
    background: var(--vscode-editor-background);
    border-top: 1px solid var(--vscode-panel-border, #444);
  }

  .section-label {
    font-size: 0.82em;
    text-transform: uppercase;
    letter-spacing: 0.05em;
    color: var(--vscode-descriptionForeground);
    margin: 6px 0 4px 0;
    opacity: 0.7;
  }

  .section-label:first-child {
    margin-top: 0;
  }

  .returns-separator {
    height: 1px;
    background: var(--vscode-panel-border, #444);
    margin: 6px 0 2px 0;
  }

  .params-list {
    display: flex;
    flex-direction: column;
    gap: 2px;
  }

  .param-row {
    display: flex;
    align-items: center;
    gap: 6px;
    padding: 2px 4px;
  }

  .param-row.nested {
    padding-left: 16px;
  }

  .param-name {
    font-family: var(--vscode-editor-font-family, monospace);
    font-size: 0.92em;
    color: var(--vscode-foreground);
  }

  .param-type {
    font-family: var(--vscode-editor-font-family, monospace);
    font-size: 0.88em;
    color: var(--l4-tok-identifier, #4ec9b0);
  }

  .param-desc {
    font-size: 0.85em;
    color: var(--vscode-descriptionForeground);
    padding: 0 4px 2px 4px;
    opacity: 0.8;
  }

  .param-desc.nested {
    padding-left: 16px;
  }

  .param-enums {
    display: flex;
    flex-wrap: wrap;
    gap: 3px;
    padding: 2px 4px;
  }

  .enum-tag {
    font-family: var(--vscode-editor-font-family, monospace);
    font-size: 0.82em;
    padding: 1px 5px;
    border-radius: 3px;
    background: var(--vscode-badge-background, #4d4d4d);
    color: var(--vscode-badge-foreground, #ccc);
  }

  .badge {
    font-size: 0.78em;
    padding: 1px 5px;
    border-radius: 3px;
    flex-shrink: 0;
  }

  .default-badge {
    background: none;
    border: none;
    padding: 0;
    color: var(--vscode-descriptionForeground);
    opacity: 0.5;
  }

  .required-badge {
    background: none;
    border: none;
    padding: 0;
    color: var(--vscode-descriptionForeground);
    opacity: 0.5;
  }

  .deontic-badge {
    background: rgba(197, 134, 192, 0.15);
    color: #c586c0;
  }

  .return-info {
    display: flex;
    align-items: center;
    gap: 6px;
    padding: 2px 4px;
  }

  .nested-props {
    border-left: 1px solid var(--vscode-panel-border, #444);
    margin-left: 8px;
    padding-left: 4px;
  }
</style>
