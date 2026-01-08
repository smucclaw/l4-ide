import type {
  QueryPlanResponse,
  Parameters,
} from '@repo/decision-service-types'

export type DecisionServiceClient = {
  baseUrl: string
}

export type FunctionMetadata = {
  name: string
  description: string
  parameters: Parameters
}

export type EvaluationResult = {
  result: unknown
  trace?: unknown
}

type FnArguments = {
  fnEvalBackend: 'jl4' | null
  fnArguments: Record<string, unknown>
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
    `${context} failed: ${response.status} ${response.statusText}${body ? ` - ${body}` : ''}`
  )
}

export async function fetchFunctionMetadata(
  client: DecisionServiceClient,
  name: string
): Promise<FunctionMetadata> {
  const encodedName = encodeURIComponent(name)
  const url = `${client.baseUrl}/functions/${encodedName}`

  const resp = await fetch(url)
  if (!resp.ok) {
    await throwWithResponseBody(resp, `GET ${url}`)
  }

  const data = await resp.json()
  return {
    name: data.function?.name ?? name,
    description: data.function?.description ?? '',
    parameters: data.function?.parameters ?? {
      type: 'object',
      properties: {},
      required: [],
    },
  }
}

export async function fetchQueryPlan(
  client: DecisionServiceClient,
  name: string,
  bindings: Record<string, unknown>
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

export async function evaluateFunction(
  client: DecisionServiceClient,
  name: string,
  args: Record<string, unknown>,
  options: { trace?: boolean } = {}
): Promise<EvaluationResult> {
  const encodedName = encodeURIComponent(name)
  const traceParam = options.trace ? '?trace=full' : ''
  const url = `${client.baseUrl}/functions/${encodedName}/evaluation${traceParam}`

  const payload: FnArguments = {
    fnEvalBackend: 'jl4',
    fnArguments: args,
  }

  const resp = await fetch(url, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(payload),
  })

  if (!resp.ok) {
    await throwWithResponseBody(resp, `POST ${url}`)
  }

  const data = await resp.json()
  return {
    result: data.result,
    trace: data.trace,
  }
}

export async function fetchGraphviz(
  client: DecisionServiceClient,
  name: string,
  bindings: Record<string, unknown>,
  format: 'svg' | 'png' = 'svg'
): Promise<string> {
  const encodedName = encodeURIComponent(name)
  const extension = format === 'svg' ? 'trace.svg' : 'trace.png'
  const url = `${client.baseUrl}/functions/${encodedName}/evaluation/${extension}?trace=full`

  const args: FnArguments = {
    fnEvalBackend: 'jl4',
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

  if (format === 'svg') {
    return await resp.text()
  } else {
    const blob = await resp.blob()
    return URL.createObjectURL(blob)
  }
}

export function createClient(baseUrl: string): DecisionServiceClient {
  return { baseUrl: baseUrl.replace(/\/$/, '') }
}
