import type { AuthManager } from './auth.js'

export interface DeployResponse {
  id: string
  status: string
  metadata?: unknown
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
    isUpdate: boolean
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

    const resp = await this.request(path, {
      method,
      body: formData,
    })

    if (!resp.ok) await throwWithBody(resp, `${method} ${path}`)
    return (await resp.json()) as DeployResponse
  }

  /**
   * Get deployment status (lightweight, no function schemas).
   */
  async getDeploymentStatus(deploymentId: string): Promise<DeployResponse> {
    const encodedId = encodeURIComponent(deploymentId)
    const resp = await this.request(`/deployments/${encodedId}`)
    if (!resp.ok) await throwWithBody(resp, `GET /deployments/${encodedId}`)
    return (await resp.json()) as DeployResponse
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
   * Get the organization-wide OpenAPI spec (all deployments).
   */
  async getOrgOpenApi(): Promise<unknown> {
    const resp = await this.request('/openapi.json')
    if (!resp.ok) await throwWithBody(resp, 'GET /openapi.json')
    return resp.json()
  }

  /**
   * Get the OpenAPI spec for a single deployment.
   */
  async getDeploymentOpenApi(deploymentId: string): Promise<unknown> {
    const encodedId = encodeURIComponent(deploymentId)
    const resp = await this.request(`/deployments/${encodedId}/openapi.json`)
    if (!resp.ok)
      await throwWithBody(resp, `GET /deployments/${encodedId}/openapi.json`)
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
