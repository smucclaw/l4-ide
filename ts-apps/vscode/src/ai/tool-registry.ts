import type { AiProxyTool } from './ai-proxy-client.js'

/**
 * OpenAI function-tool declarations for the built-in client-side
 * tools. These ship to the ai-proxy in the `tools` field of the chat
 * request; the proxy passes them through to the provider and streams
 * back tool_call deltas when the model invokes one.
 *
 * Names use `category__name` with a double-underscore separator so a
 * prefix split gives the dispatcher the right category. Third-party
 * MCP tools later namespace as `{serverId}__{toolName}` and land in
 * the same switch.
 */
export const BUILTIN_TOOLS: AiProxyTool[] = [
  {
    type: 'function',
    function: {
      name: 'fs__read_file',
      description:
        'Read a UTF-8 text file, or list the contents of a directory. When `path` points at a file, returns the file contents as a string. When `path` points at a directory, returns a JSON object with `entries[]` (up to 100 names per page, directories first), plus `total`, `from`, `count`, `hasMore`, and `nextFrom`. To walk a large folder, re-call with `from: <previous nextFrom>`. `.git`, `node_modules`, and `.DS_Store` are filtered out.',
      parameters: {
        type: 'object',
        additionalProperties: false,
        properties: {
          path: {
            type: 'string',
            description:
              'Absolute path, or path relative to a loaded workspace folder. Only files inside a loaded workspace folder can be read/edited/deleted — if editor-context marks the active file as outside the workspace, tell the user to add its folder first. Use `.` for the workspace root.',
          },
          from: {
            type: 'number',
            description:
              'Optional. When `path` is a directory, skip this many sorted entries before reading the next 100. Use the `nextFrom` value returned by a previous call to page through larger folders. Ignored for file reads.',
          },
        },
        required: ['path'],
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'fs__create_file',
      description:
        'Create a new file in the workspace with the supplied contents. Fails if the file already exists. Use fs__edit_file to modify an existing file.',
      parameters: {
        type: 'object',
        additionalProperties: false,
        properties: {
          path: {
            type: 'string',
            description: 'Workspace-relative path for the new file.',
          },
          content: {
            type: 'string',
            description: 'Full contents of the new file, UTF-8 encoded.',
          },
        },
        required: ['path', 'content'],
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'fs__edit_file',
      description:
        'Edit an existing file by string-anchored find/replace. `old` must appear in the file EXACTLY ONCE — include surrounding context lines if the raw snippet is not unique. Replaces that single occurrence with `new`. Fails if `old` is missing or ambiguous.',
      parameters: {
        type: 'object',
        additionalProperties: false,
        properties: {
          path: {
            type: 'string',
            description: 'Workspace-relative path to the file to edit.',
          },
          old: {
            type: 'string',
            description:
              'Exact text to replace. Must be unique in the file; include context if the natural snippet repeats.',
          },
          new: {
            type: 'string',
            description: 'Replacement text.',
          },
        },
        required: ['path', 'old', 'new'],
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'fs__delete_file',
      description:
        'Delete a file from the workspace. Sends it to the system Trash rather than `unlink`-ing, so the user can recover accidental deletions. Requires user confirmation by default.',
      parameters: {
        type: 'object',
        additionalProperties: false,
        properties: {
          path: {
            type: 'string',
            description: 'Workspace-relative path to the file to delete.',
          },
        },
        required: ['path'],
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'lsp__diagnostics',
      description:
        'Fetch the current type-check / parse diagnostics the L4 language server has for a file, as a JSON report. Use this after every fs__edit_file or fs__create_file to verify the file still type-checks. Returns `total`, per-severity `counts`, and a `diagnostics[]` list with 1-indexed line/column, severity, message, source, and code. Empty `diagnostics[]` means the file is clean.',
      parameters: {
        type: 'object',
        additionalProperties: false,
        properties: {
          path: {
            type: 'string',
            description:
              'Absolute path, or path relative to a loaded workspace folder.',
          },
          source: {
            type: 'string',
            description:
              'Optional. Restrict results to diagnostics reported by a specific source, e.g. `"jl4"` for the L4 language server. Omit to include all sources VSCode knows about.',
          },
        },
        required: ['path'],
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'l4__evaluate',
      description:
        "Return the L4 language server's most recent evaluation results for every `#EVAL` / `#CHECK` / `#TRACE` directive written into the source of a file. Returns `{ path, count, results[{ directiveId, line, success, value }] }`. Use this after `fs__edit_file` to confirm the runtime behaviour of existing directives. `success: false` means the directive evaluated to `False` (for CHECK) or threw; `value` is the pretty-printed result.",
      parameters: {
        type: 'object',
        additionalProperties: false,
        properties: {
          path: {
            type: 'string',
            description:
              'Absolute path, or path relative to a loaded workspace folder.',
          },
          timeoutMs: {
            type: 'number',
            description:
              'Optional. Max wait (ms) for the LSP to finish a fresh compile before returning an empty result set. Default 6000, capped at 15000.',
          },
        },
        required: ['path'],
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'meta__ask_user',
      description:
        'Ask the user a clarifying question and wait for their reply before continuing. Use this sparingly — only when legal text is genuinely ambiguous and you cannot proceed without a decision. Prefer asking multiple related questions in one call. The interactive question panel is still being wired up; until then this returns a hint that the user was not actually prompted, so rely on your best judgement and ask the user in your assistant text if you truly need input.',
      parameters: {
        type: 'object',
        additionalProperties: false,
        properties: {
          question: {
            type: 'string',
            description:
              'The question to surface to the user, phrased concisely.',
          },
          choices: {
            type: 'array',
            items: { type: 'string' },
            description:
              'Optional fixed set of choices. Omit for free-form answers.',
          },
        },
        required: ['question'],
      },
    },
  },
]
