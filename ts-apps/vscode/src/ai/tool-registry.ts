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
        'Read a file or list a directory, one line per entry. Response prefixed with `[<path> <start>-<end>/<total>]` (or `[<path> keywords="…" matches=N chunks=K/M]` with `search_keywords`); when `<end> < <total>` more lines remain — call again with `startLine=<end>+1`. Hard cap 500 lines / 4000 chars per call. Directories: `name/` for subdirs, directories first. `.git`, `node_modules`, `.DS_Store` hidden.',
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
            description: '1-based inclusive, default startLine+499.',
          },
          search_keywords: {
            type: 'string',
            description:
              'One or more keywords separated by whitespace; a line is a hit if it matches ANY of them (OR). Each keyword is a case-insensitive regex with literal-substring fallback. For files, matches carry 2 lines of context (hits prefixed `>>>`, context `   `, chunks joined `---`). For directories, the tree is walked recursively and file CONTENTS are grepped — results emitted as `<path>:<lineno>: <text>` rows.',
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
        'Create a file seeded with a single line `// new file content` (for `.html`/`.htm` files: a minimal blank-white HTML skeleton whose body reads `New document in progress ...`, opened in the built-in browser preview instead of a source tab). Fails if the file already exists. To fill it, follow up with fs__edit_file calls: the first replaces the seed line (or, for HTML, the `New document in progress ...` placeholder) with the initial content; further calls add more sections incrementally. Do NOT try to write the whole file in one giant edit — split into smaller chunks (see fs__edit_file).',
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
        'String-anchored find/replace. Without `startLine`, `old` must appear in the file EXACTLY ONCE — include surrounding context lines if the natural snippet repeats. With `startLine`, the first occurrence after that line is taken. **`old` must be a non-empty anchor; whole-file replacement is not supported.** For a freshly-created file, anchor the first edit on the seed line `// new file content`, then add more content via further fs__edit_file calls. **Prefer smaller edits — tens of lines per call.** A single edit with a huge `new` payload risks the model hitting max_tokens mid-stream, leaving the file untouched; multiple smaller edits sidestep that entirely.',
      parameters: {
        type: 'object',
        additionalProperties: false,
        properties: {
          path: { type: 'string', description: 'Workspace path.' },
          old: {
            type: 'string',
            description:
              'Exact non-empty text to replace. Must be unique in the file unless `startLine` is set. Empty string is rejected — split big rewrites into multiple smaller fs__edit_file calls.',
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
      name: 'l4__refactor',
      description:
        "Apply a structured L4 refactor. The `action` discriminator selects the operation; more actions will be added over time. Today: `rename` substitutes an identifier across the file AND every file that IMPORTs it (driven by the LSP's references provider, so the file must currently type-check — use l4__evaluate first if unsure). Preserves backtick quoting per-occurrence: a source `\\`old name\\`` becomes `\\`new name\\``; if the new name contains spaces or punctuation it's wrapped in backticks everywhere automatically. Pass identifier names WITHOUT surrounding backticks. Prefer this tool over hand-rolling cross-file find/replace via fs__edit_file.",
      parameters: {
        type: 'object',
        additionalProperties: false,
        properties: {
          action: {
            type: 'string',
            enum: ['rename'],
            description:
              'Which refactor to apply. Supported: `rename` — substitute an identifier across the file and every importer. Reserved for future actions (extract, inline, …); pass exactly one of the listed enum values.',
          },
          path: {
            type: 'string',
            description:
              'Workspace path of any L4 file the action anchors on. For `rename`, this is a file containing the identifier (usually where it is DEFINED); the rename then propagates through all importers.',
          },
          oldName: {
            type: 'string',
            description:
              "Required when action='rename'. Current identifier (no surrounding backticks). Must appear in the source of `path`.",
          },
          newName: {
            type: 'string',
            description:
              "Required when action='rename'. New identifier (no surrounding backticks). May contain spaces/punctuation — backticks are added automatically where required.",
          },
        },
        required: ['action', 'path'],
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'l4__evaluate',
      description:
        'Type-check and run `#EVAL`/`#CHECK`/`#TRACE`. Returns errors if not clean, else directive results.',
      parameters: {
        type: 'object',
        additionalProperties: false,
        properties: {
          path: { type: 'string', description: 'Workspace path.' },
          timeoutMs: {
            type: 'number',
            description: 'Max compile wait ms (default 6000, cap 15000).',
          },
          mode: {
            type: 'string',
            enum: ['changed', 'full'],
            description:
              '`full` (default) prints every directive value. `changed` prints only directives added or whose value changed since the last l4__evaluate call; collapses to a single count line when nothing moved.',
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
        'Stream a brief progress update to the user mid-turn. The text renders inline as plain assistant prose (not as a tool-call card). Use during long multi-step tasks (doc lookups, validation loops, multi-file drafting) Keep it under ~120 characters, present tense, no markdown headings. Do not repeat same status, and do not duplicate final answer.',
      parameters: {
        type: 'object',
        additionalProperties: false,
        properties: {
          text: {
            type: 'string',
            description:
              'Short user-facing status sentence. Plain prose. Will be appended to the assistant message as if written it inline.',
          },
        },
        required: ['text'],
      },
    },
  },
]
