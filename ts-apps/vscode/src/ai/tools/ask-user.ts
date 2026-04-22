/**
 * meta__ask_user — surface a structured question to the user and
 * pause the turn until they answer. The dispatcher receives a
 * `requestAnswer(callId, question, choices)` callback that resolves
 * once the webview posts `AiChatAnswerUser`. This file just adapts
 * the tool args into that callback's shape and formats the result
 * the LLM sees.
 */

export interface AskUserArgs {
  question: string
  choices?: string[]
}

export type AskUserAdapter = (
  callId: string,
  question: string,
  choices: string[] | undefined
) => Promise<string>

export async function metaAskUser(
  callId: string,
  args: AskUserArgs,
  ask: AskUserAdapter
): Promise<string> {
  if (!args.question || typeof args.question !== 'string') {
    throw new Error('meta__ask_user: `question` is required')
  }
  const answer = await ask(callId, args.question, args.choices)
  // Always return JSON so the LLM can branch on `answered` without
  // string-matching. An empty `answer` means the user skipped.
  if (!answer) {
    return JSON.stringify({
      answered: false,
      note: 'The user skipped this question — proceed with your best guess.',
    })
  }
  return JSON.stringify({ answered: true, answer })
}
