#!/usr/bin/env node
import { chromium } from 'playwright'

console.log('🔍 Starting LSP client diagnostic...\n')

const browser = await chromium.launch()
const context = await browser.newContext()
const page = await context.newPage()

// Capture all console messages
const consoleLogs = []
page.on('console', (msg) => {
  const text = msg.text()
  const type = msg.type()
  consoleLogs.push({ type, text, timestamp: Date.now() })

  // Print interesting messages in real-time
  if (
    text.includes('WebSocket') ||
    text.includes('LSP') ||
    text.includes('runClient') ||
    text.includes('socket') ||
    text.includes('ws://') ||
    text.includes('language') ||
    text.includes('client')
  ) {
    console.log(`[${type.toUpperCase()}] ${text}`)
  }
})

// Capture network requests to WebSocket
page.on('websocket', (ws) => {
  console.log(`🔌 WebSocket connection attempt: ${ws.url()}`)

  ws.on('framereceived', (event) => {
    console.log(`📨 WebSocket frame received: ${event.payload.slice(0, 100)}`)
  })

  ws.on('framesent', (event) => {
    console.log(`📤 WebSocket frame sent: ${event.payload.slice(0, 100)}`)
  })

  ws.on('close', () => {
    console.log('🔴 WebSocket connection closed')
  })
})

// Capture page errors
page.on('pageerror', (error) => {
  console.log(`❌ Page error: ${error.message}`)
})

console.log('📄 Loading page: http://localhost:5173/\n')
await page.goto('http://localhost:5173/', { waitUntil: 'networkidle' })

console.log('⏳ Waiting for page initialization (10 seconds)...\n')
await page.waitForTimeout(10000)

console.log('\n📊 Diagnostic Summary:')
console.log('='.repeat(60))

// Check if Monaco editor loaded
const editorExists = await page.evaluate(() => {
  return typeof window.monaco !== 'undefined'
})
console.log(`✓ Monaco loaded: ${editorExists}`)

// Check if editor element exists
const editorElement = await page.locator('.monaco-editor').count()
console.log(`✓ Monaco editor elements: ${editorElement}`)

// Check environment variables
const envVars = await page.evaluate(() => {
  return {
    sessionUrl: import.meta.env.VITE_SESSION_URL,
    socketUrl: import.meta.env.VITE_SOCKET_URL,
  }
})
console.log(`✓ VITE_SESSION_URL: ${envVars.sessionUrl || 'NOT SET'}`)
console.log(`✓ VITE_SOCKET_URL: ${envVars.socketUrl || 'NOT SET'}`)

// Count console messages by type
const wsMessages = consoleLogs.filter(
  (l) =>
    l.text.toLowerCase().includes('websocket') ||
    l.text.toLowerCase().includes('ws://')
)
const lspMessages = consoleLogs.filter(
  (l) =>
    l.text.toLowerCase().includes('lsp') ||
    l.text.toLowerCase().includes('language')
)
const errors = consoleLogs.filter((l) => l.type === 'error')

console.log(`\n📝 Console Activity:`)
console.log(`  - Total console messages: ${consoleLogs.length}`)
console.log(`  - WebSocket-related: ${wsMessages.length}`)
console.log(`  - LSP-related: ${lspMessages.length}`)
console.log(`  - Errors: ${errors.length}`)

if (wsMessages.length > 0) {
  console.log(`\n🔌 WebSocket Messages:`)
  wsMessages.forEach((m) => console.log(`  [${m.type}] ${m.text}`))
}

if (lspMessages.length > 0) {
  console.log(`\n🔧 LSP Messages:`)
  lspMessages.forEach((m) => console.log(`  [${m.type}] ${m.text}`))
}

if (errors.length > 0) {
  console.log(`\n❌ Errors:`)
  errors.slice(0, 5).forEach((m) => console.log(`  ${m.text}`))
}

console.log('\n' + '='.repeat(60))
console.log('✅ Diagnostic complete\n')

await browser.close()
