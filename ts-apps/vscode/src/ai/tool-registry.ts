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
        'Read a file or list a directory, one line per entry. Response prefixed with `[<path> <start>-<end>/<total>]` (or `[<path> pattern="…" matches=N chunks=K/M]` with `pattern`); a trailing `, next startLine=<n>` marks more available. Hard cap 100 lines / 4000 chars per call. Directories: `name/` for subdirs, directories first. `.git`, `node_modules`, `.DS_Store` hidden.',
      parameters: {
        type: 'object',
        additionalProperties: false,
        properties: {
          path: {
            type: 'string',
            description: 'Workspace path. Use `.` for the workspace root.',
          },
          startLine: { type: 'number', description: '1-based, default 1.' },
          endLine: {
            type: 'number',
            description: '1-based inclusive, default startLine+99.',
          },
          pattern: {
            type: 'string',
            description:
              'Case-insensitive regex; literal-substring fallback. For files, matches carry 2 lines of context (hits prefixed `>>>`, context `   `, chunks joined `---`). For directories, matching entries only.',
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
        'Create a file seeded with a single line "// new file content". Fails if the file already exists. To fill it, follow up with fs__edit_file — either anchor on the exact seed line or pass old:"" to overwrite the whole file.',
      parameters: {
        type: 'object',
        additionalProperties: false,
        properties: {
          path: { type: 'string', description: 'Workspace path.' },
        },
        required: ['path'],
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'fs__edit_file',
      description:
        'String-anchored find/replace. Without `startLine`, `old` must appear in the file EXACTLY ONCE — include surrounding context lines if the natural snippet repeats. With `startLine`, the first occurrence after that line is taken. Pass `old: ""` to replace the ENTIRE file with `new` — use this right after fs__create_file to fill the new file with content.',
      parameters: {
        type: 'object',
        additionalProperties: false,
        properties: {
          path: { type: 'string', description: 'Workspace path.' },
          old: {
            type: 'string',
            description:
              'Exact text to replace. Must be unique in the file unless `startLine` is set. Pass an empty string ("") to overwrite the entire file with `new`.',
          },
          new: {
            type: 'string',
            description:
              'Replacement text. Pass "" to delete the `old` snippet.',
          },
          startLine: {
            type: 'number',
            description:
              'Optional 1-based line to anchor the search — First occurrence of `old` strictly after this line. Leave some buffer (~5-10 lines)',
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
        'Moves the file to the system Trash (recoverable). Requires user confirmation by default.',
      parameters: {
        type: 'object',
        additionalProperties: false,
        properties: {
          path: { type: 'string', description: 'Workspace path.' },
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
        "Fetch the L4 language server's current diagnostics for a file. Call after every fs__edit_file / fs__create_file to verify the file still type-checks. Returns `{ total, counts, diagnostics[{ line, column, severity, message, source, code }] }` with 1-indexed positions; empty `diagnostics[]` = clean.",
      parameters: {
        type: 'object',
        additionalProperties: false,
        properties: {
          path: { type: 'string', description: 'Workspace path.' },
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
        "Returns the L4 server's latest results for every `#EVAL` / `#CHECK` / `#TRACE` directive in the file: `{ path, count, results[{ directiveId, line, success, value }] }`. Runs lsp diagnostics first internally.",
      parameters: {
        type: 'object',
        additionalProperties: false,
        properties: {
          path: { type: 'string', description: 'Workspace path.' },
          timeoutMs: {
            type: 'number',
            description:
              'Max ms to wait for a fresh compile before returning empty. Default 6000, max 15000.',
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
        'Ask the user a clarifying question and wait for their reply. Use sparingly — only when the task is genuinely ambiguous and you cannot proceed without a decision.',
      parameters: {
        type: 'object',
        additionalProperties: false,
        properties: {
          question: {
            type: 'string',
            description: 'Concise question text.',
          },
          choices: {
            type: 'array',
            items: { type: 'string' },
            description: 'Optional fixed choices. Omit for free-form answers.',
          },
        },
        required: ['question'],
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'meta__post_status_update',
      description:
        'Stream a brief progress update to the user mid-turn. The text renders inline in the chat as plain assistant prose (not as a tool-call card). Use during long multi-step tasks (doc lookups, validation loops, multi-file drafting) so the user sees what you are doing instead of a silent spinner. Keep it under ~120 characters, present tense, no markdown headings or bullets. Do not repeat the same status, and do not duplicate your final answer.',
      parameters: {
        type: 'object',
        additionalProperties: false,
        properties: {
          text: {
            type: 'string',
            description:
              'Short user-facing status sentence. Plain prose. Will be appended to the assistant message as if you had written it inline.',
          },
        },
        required: ['text'],
      },
    },
  },
]
