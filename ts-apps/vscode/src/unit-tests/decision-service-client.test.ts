import assert from 'node:assert/strict'
import test from 'node:test'
import {
  fetchQueryPlan,
  upsertFunctionFromSource,
} from '../decision-service-client.js'

test('upsertFunctionFromSource falls back to POST after 404', async () => {
  const originalFetch = globalThis.fetch
  const calls: Array<{ url: string; init?: RequestInit }> = []

  globalThis.fetch = (async (url: string, init?: RequestInit) => {
    calls.push({ url, init })
    if (calls.length === 1) {
      return new Response('', { status: 404, statusText: 'Not Found' })
    }
    return new Response('', { status: 200 })
  }) as typeof fetch

  try {
    await upsertFunctionFromSource(
      { baseUrl: 'http://example.test' },
      'MyFn',
      'def MyFn: Boolean = True'
    )
  } finally {
    globalThis.fetch = originalFetch
  }

  assert.equal(calls.length, 2)
  assert.equal(calls[0]?.init?.method, 'PUT')
  assert.equal(calls[1]?.init?.method, 'POST')
})

test('fetchQueryPlan POSTs and parses JSON', async () => {
  const originalFetch = globalThis.fetch
  const calls: Array<{ url: string; init?: RequestInit }> = []
  const expected = {
    determined: null,
    stillNeeded: [],
    ranked: [],
    inputs: [],
    asks: [],
    impact: {},
    impactByAtomId: {},
    note: '',
    ladder: null,
  }

  globalThis.fetch = (async (url: string, init?: RequestInit) => {
    calls.push({ url, init })
    return new Response(JSON.stringify(expected), {
      status: 200,
      headers: { 'Content-Type': 'application/json' },
    })
  }) as typeof fetch

  let resp: unknown
  try {
    resp = await fetchQueryPlan({ baseUrl: 'http://example.test' }, 'MyFn', {
      'x > 0': true,
    })
  } finally {
    globalThis.fetch = originalFetch
  }

  assert.deepEqual(resp, expected)
  assert.equal(calls.length, 1)
  assert.equal(calls[0]?.init?.method, 'POST')
})
