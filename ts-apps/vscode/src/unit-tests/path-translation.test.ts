import { test, describe } from 'node:test'
import * as assert from 'node:assert/strict'
import { canonicalToApiHostPath } from '../api-host-path.js'

describe('canonicalToApiHostPath', () => {
  test('rewrites /deployments collection to the empty path', () => {
    assert.equal(canonicalToApiHostPath('/deployments'), '')
    assert.equal(
      canonicalToApiHostPath('/deployments?functions=full'),
      '?functions=full'
    )
  })

  test('passes /openapi.json through unchanged', () => {
    assert.equal(canonicalToApiHostPath('/openapi.json'), '/openapi.json')
  })

  test('strips /deployments from per-deployment paths', () => {
    assert.equal(canonicalToApiHostPath('/deployments/abc'), '/abc')
    assert.equal(
      canonicalToApiHostPath('/deployments/abc?functions=full'),
      '/abc?functions=full'
    )
    assert.equal(
      canonicalToApiHostPath('/deployments/abc/openapi.json'),
      '/abc/openapi.json'
    )
    assert.equal(canonicalToApiHostPath('/deployments/abc/files'), '/abc/files')
    assert.equal(
      canonicalToApiHostPath('/deployments/abc/updates/job-9'),
      '/abc/updates/job-9'
    )
  })

  test('rewrites /functions to /fn only at the deployment-id boundary', () => {
    assert.equal(
      canonicalToApiHostPath('/deployments/abc/functions'),
      '/abc/fn'
    )
    assert.equal(
      canonicalToApiHostPath('/deployments/abc/functions/is%20eligible'),
      '/abc/fn/is%20eligible'
    )
    assert.equal(
      canonicalToApiHostPath('/deployments/abc/functions/fn1/evaluation'),
      '/abc/fn/fn1/evaluation'
    )
    assert.equal(
      canonicalToApiHostPath('/deployments/abc/functions/fn1/evaluation/batch'),
      '/abc/fn/fn1/evaluation/batch'
    )
    assert.equal(
      canonicalToApiHostPath('/deployments/abc/functions/fn1/query-plan'),
      '/abc/fn/fn1/query-plan'
    )
    assert.equal(
      canonicalToApiHostPath(
        '/deployments/abc/functions/fn1/state-graphs/main'
      ),
      '/abc/fn/fn1/state-graphs/main'
    )
  })

  test('preserves query strings', () => {
    assert.equal(
      canonicalToApiHostPath('/deployments/abc/files?identifier=foo'),
      '/abc/files?identifier=foo'
    )
    assert.equal(
      canonicalToApiHostPath('/deployments/abc/functions?functions=full'),
      '/abc/fn?functions=full'
    )
  })

  test('leaves non-/deployments paths untouched', () => {
    assert.equal(canonicalToApiHostPath('/health'), '/health')
    assert.equal(canonicalToApiHostPath('/.well-known/x'), '/.well-known/x')
  })
})
