/**
 * Rewrite a canonical jl4-service dataplane path to the consolidated api
 * host's path-slug scheme (jl4-auth-proxy/src/routing/parse.ts ::
 * parseApiHostPath). The base URL already carries the /{slug} prefix.
 *
 *   /deployments                                  →  ''
 *   /deployments?functions=full                   →  '?functions=full'
 *   /openapi.json                                 →  '/openapi.json'
 *   /deployments/{id}                             →  '/{id}'
 *   /deployments/{id}/openapi.json                →  '/{id}/openapi.json'
 *   /deployments/{id}/files[/...]                 →  '/{id}/files[/...]'
 *   /deployments/{id}/updates/{jobId}             →  '/{id}/updates/{jobId}'
 *   /deployments/{id}/functions[/{fn}[/{action}]] →  '/{id}/fn[/{fn}[/{action}]]'
 *
 * Anything that doesn't start with `/deployments` is returned unchanged —
 * the api host doesn't route those, and the call would 404 either way.
 *
 * Lives in its own zero-dependency module so `node --test` can exercise it
 * without dragging the vscode runtime through the import graph.
 */
export function canonicalToApiHostPath(canonical: string): string {
  const [path, query] = canonical.split('?', 2)
  const qs = query ? `?${query}` : ''
  if (!path) return qs
  if (path === '/openapi.json') return path + qs
  if (path === '/deployments') return qs
  if (path.startsWith('/deployments/')) {
    const rest = path.slice('/deployments'.length) // /{id}[/...]
    // /{id}/functions[/...] → /{id}/fn[/...]
    return rest.replace(/^(\/[^/]+)\/functions(\/|$)/, '$1/fn$2') + qs
  }
  return canonical
}
