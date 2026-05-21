import type { AuthManager } from './auth.js'

export interface DeployResponse {
  id: string
  status: string
  metadata?: unknown
  error?: string
  /** Set by POST/PUT: id of the async deploy/update job to poll. */
  updateId?: string
}

/** Status of an async deploy/update job. */
export interface UpdateStatusResponse {
  updateId: string
  deploymentId: string
  status: 'compiling' | 'applied' | 'rejected'
  error?: string
}

export interface ServiceHealth {
  status: string
  instances?: unknown[]
}

async function throwWithBody(resp: Response, context: string): Promise<never> {
  let body = ''
  try {
    body = await resp.text()
  } catch {
    body = ''
  }
  throw new Error(
    `${context}: ${resp.status} ${resp.statusText}${body ? ` — ${body}` : ''}`
  )
}

/**
 * REST client for jl4-service behind the auth proxy.
 *
 * All requests use the AuthManager for credentials and service URL resolution.
 */
export class ServiceClient {
  constructor(private readonly auth: AuthManager) {}

  private async request(path: string, init?: RequestInit): Promise<Response> {
    const serviceUrl = this.auth.getEffectiveServiceUrl()
    if (!serviceUrl) {
      throw new Error(
        'No service URL configured. Set jl4.serviceUrl in settings.'
      )
    }

    const headers = await this.auth.getAuthHeaders()
    const url = `${serviceUrl.replace(/\/$/, '')}${path}`

    return fetch(url, {
      ...init,
      headers: {
        ...headers,
        ...init?.headers,
      },
    })
  }

  /**
   * Deploy L4 rules as a ZIP file.
   *
   * For new deployments, POST /deployments.
   * For updating existing deployments, PUT /deployments/{id}.
   */
  async deploy(
    deploymentId: string,
    zipBuffer: Uint8Array,
    isUpdate: boolean,
    description?: string
  ): Promise<DeployResponse> {
    const blob = new Blob([zipBuffer], { type: 'application/zip' })
    const formData = new FormData()
    formData.append('sources', blob, 'sources.zip')

    const encodedId = encodeURIComponent(deploymentId)
    const method = isUpdate ? 'PUT' : 'POST'
    const path = isUpdate ? `/deployments/${encodedId}` : '/deployments'

    // For POST, include the deployment ID in the form data
    if (!isUpdate) {
      formData.append('id', deploymentId)
    }

    // Operator-supplied "Intended use". Omitted (not sent
    // empty) when blank so a source-only PUT preserves the stored value.
    if (description && description.trim().length > 0) {
      formData.append('description', description.trim())
    }

    const resp = await this.request(path, {
      method,
      body: formData,
    })

    if (!resp.ok) await throwWithBody(resp, `${method} ${path}`)
    return (await resp.json()) as DeployResponse
  }

  /**
   * Get deployment status. `mode` controls how much detail the response's
   * `metadata.functions[]` carries:
   *   - `'simple'` (default): name + description only — cheap status poll.
   *   - `'full'`: full per-function parameter + returnSchema in one round-trip
   *     (avoids fanning out per-function `getFunctionSchema` calls).
   *   - `'none'`: omit the functions array entirely.
   */
  async getDeploymentStatus(
    deploymentId: string,
    mode: 'simple' | 'full' | 'none' = 'simple'
  ): Promise<DeployResponse> {
    const encodedId = encodeURIComponent(deploymentId)
    const query = mode === 'simple' ? '' : `?functions=${mode}`
    const resp = await this.request(`/deployments/${encodedId}${query}`)
    if (!resp.ok)
      await throwWithBody(resp, `GET /deployments/${encodedId}${query}`)
    return (await resp.json()) as DeployResponse
  }

  /**
   * Poll an async deploy/update job. Independent of the deployment's
   * own status — the live version is unaffected until the job applies.
   */
  async getUpdateStatus(
    deploymentId: string,
    updateId: string
  ): Promise<UpdateStatusResponse> {
    const encodedId = encodeURIComponent(deploymentId)
    const encodedJob = encodeURIComponent(updateId)
    const resp = await this.request(
      `/deployments/${encodedId}/updates/${encodedJob}`
    )
    if (!resp.ok)
      await throwWithBody(
        resp,
        `GET /deployments/${encodedId}/updates/${encodedJob}`
      )
    return (await resp.json()) as UpdateStatusResponse
  }

  /**
   * Delete (undeploy) a deployment.
   */
  async undeploy(deploymentId: string): Promise<void> {
    const encodedId = encodeURIComponent(deploymentId)
    const resp = await this.request(`/deployments/${encodedId}`, {
      method: 'DELETE',
    })
    if (!resp.ok) await throwWithBody(resp, `DELETE /deployments/${encodedId}`)
  }

  /**
   * List all deployments with full function details.
   * Used by the sidebar to populate the deployment tree.
   */
  async getDeployments(): Promise<unknown[]> {
    const resp = await this.request('/deployments?functions=full')
    if (resp.status === 403) return [] // insufficient permissions — handled by caller
    if (!resp.ok) await throwWithBody(resp, 'GET /deployments?functions=full')
    return resp.json() as Promise<unknown[]>
  }

  /**
   * Fetch the source files of a deployment as
   * `{ files: [{ path, totalLines, exports, content }] }`.
   * Used to materialise a deployment to disk via the sidebar Download
   * action. Backed by `GET /deployments/{id}/files` (no query params),
   * which returns every `.l4` source in a single response.
   */
  async getDeploymentFiles(deploymentId: string): Promise<{
    files: Array<{
      path: string
      totalLines: number
      exports: string[]
      content: string
    }>
  }> {
    const encodedId = encodeURIComponent(deploymentId)
    const resp = await this.request(`/deployments/${encodedId}/files`)
    if (!resp.ok)
      await throwWithBody(resp, `GET /deployments/${encodedId}/files`)
    return (await resp.json()) as {
      files: Array<{
        path: string
        totalLines: number
        exports: string[]
        content: string
      }>
    }
  }

  /**
   * Get the OpenAPI 3.0 spec for a single deployment.
   */
  async getDeploymentOpenApi(deploymentId: string): Promise<unknown> {
    const encodedId = encodeURIComponent(deploymentId)
    const resp = await this.request(`/deployments/${encodedId}/openapi.json`)
    if (!resp.ok)
      await throwWithBody(resp, `GET /deployments/${encodedId}/openapi.json`)
    return resp.json()
  }

  /**
   * Get a single function's schema. Returns the raw JSON shape of
   * jl4-service's `FunctionSummary` — `parameters` plus the structured
   * `returnSchema` (when present), with `x-l4-type` annotations on
   * record/enum nodes. Used by the chat tool-call card to render
   * arguments back into L4 syntax.
   */
  async getFunctionSchema(
    deploymentId: string,
    functionName: string
  ): Promise<unknown> {
    const encodedId = encodeURIComponent(deploymentId)
    const encodedFn = encodeURIComponent(functionName)
    const resp = await this.request(
      `/deployments/${encodedId}/functions/${encodedFn}`
    )
    if (!resp.ok)
      await throwWithBody(
        resp,
        `GET /deployments/${encodedId}/functions/${encodedFn}`
      )
    return resp.json()
  }

  /**
   * Health check.
   */
  async getHealth(): Promise<ServiceHealth> {
    const resp = await this.request('/health')
    if (!resp.ok) await throwWithBody(resp, 'GET /health')
    return (await resp.json()) as ServiceHealth
  }
}
