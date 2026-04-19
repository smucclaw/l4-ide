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
        'Read a UTF-8 text file from the user’s workspace. Returns the file contents as a string. Paths are resolved relative to the workspace root; absolute paths outside the workspace are rejected.',
      parameters: {
        type: 'object',
        additionalProperties: false,
        properties: {
          path: {
            type: 'string',
            description:
              'Workspace-relative (or workspace-rooted absolute) path to the file to read.',
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
]
