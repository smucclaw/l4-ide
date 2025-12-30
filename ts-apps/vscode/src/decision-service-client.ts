import type { QueryPlanResponse } from '@repo/decision-service-types'

export type DecisionServiceClient = {
  baseUrl: string
}

type EvalBackend = 'jl4'

type Parameters = {
  type: 'object'
  properties: Record<string, unknown>
  required: string[]
}

type FunctionDeclaration = {
  name: string
  description: string
  parameters: Parameters
  supportedEvalBackend: EvalBackend[]
}

type FunctionImplementation = {
  declaration: FunctionDeclaration
  implementation: Record<EvalBackend, string>
}

type FnArguments = {
  fnEvalBackend: EvalBackend | null
  fnArguments: Record<string, unknown>
}

function emptyParameters(): Parameters {
  return { type: 'object', properties: {}, required: [] }
}

function mkFunctionImplementation(
  name: string,
  jl4Source: string
): FunctionImplementation {
  return {
    declaration: {
      name,
      description: '',
      parameters: emptyParameters(),
      supportedEvalBackend: ['jl4'],
    },
    implementation: { jl4: jl4Source },
  }
}

async function throwWithResponseBody(
  response: Response,
  context: string
): Promise<never> {
  let body = ''
  try {
    body = await response.text()
  } catch {
    body = ''
  }
  throw new Error(
    `${context} failed: ${response.status} ${response.statusText}${body ? ` — ${body}` : ''}`
  )
}

export async function upsertFunctionFromSource(
  client: DecisionServiceClient,
  name: string,
  jl4Source: string
): Promise<void> {
  const payload = mkFunctionImplementation(name, jl4Source)
  const encodedName = encodeURIComponent(name)
  const putUrl = `${client.baseUrl}/functions/${encodedName}`

  const putResp = await fetch(putUrl, {
    method: 'PUT',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(payload),
  })
  if (putResp.ok) return

  if (putResp.status !== 404) {
    await throwWithResponseBody(putResp, `PUT ${putUrl}`)
  }

  const postUrl = `${client.baseUrl}/functions/${encodedName}`
  const postResp = await fetch(postUrl, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(payload),
  })
  if (postResp.ok) return
  await throwWithResponseBody(postResp, `POST ${postUrl}`)
}

export async function fetchQueryPlan(
  client: DecisionServiceClient,
  name: string,
  bindings: Record<string, boolean>
): Promise<QueryPlanResponse> {
  const encodedName = encodeURIComponent(name)
  const url = `${client.baseUrl}/functions/${encodedName}/query-plan`

  const args: FnArguments = {
    fnEvalBackend: null,
    fnArguments: bindings,
  }

  const resp = await fetch(url, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(args),
  })

  if (!resp.ok) {
    await throwWithResponseBody(resp, `POST ${url}`)
  }

  return (await resp.json()) as QueryPlanResponse
}
