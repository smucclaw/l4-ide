#!/usr/bin/env node
// Enhanced diagnostic to check WebSocket connection

import { chromium } from 'playwright'

const url = 'http://localhost:5173/'
console.log(`\n🔍 Checking LSP WebSocket connection...\n`)

const browser = await chromium.launch({ headless: true })
const context = await browser.newContext()
const page = await context.newPage()

let wsAttempts = []

// Capture console for WebSocket info
page.on('console', (msg) => {
  const text = msg.text()
  if (
    text.includes('WebSocket') ||
    text.includes('ws://') ||
    text.includes('LSP') ||
    text.includes('8000')
  ) {
    console.log(`📝 ${text}`)
  }
})

// Monitor WebSocket creations
await page.route('**/*', (route) => {
  const url = route.request().url()
  if (url.startsWith('ws://')) {
    wsAttempts.push(url)
    console.log(`🔌 WebSocket attempt: ${url}`)
  }
  route.continue()
})

try {
  await page.goto(url, { waitUntil: 'networkidle', timeout: 10000 })
  await page.waitForTimeout(5000)

  // Execute in browser context to check WebSocket
  const wsInfo = await page.evaluate(() => {
    return {
      socketUrl: window.localStorage.getItem('websocketUrl') || 'not found',
      env: {
        SESSION_URL: import.meta.env?.VITE_SESSION_URL || 'undefined',
        SOCKET_URL: import.meta.env?.VITE_SOCKET_URL || 'undefined',
      },
    }
  })

  console.log('\n📊 Configuration:')
  console.log(`   VITE_SOCKET_URL: ${wsInfo.env.SOCKET_URL}`)
  console.log(`   WebSocket attempts: ${wsAttempts.length}`)
  if (wsAttempts.length > 0) {
    wsAttempts.forEach((ws) => console.log(`     - ${ws}`))
  }
} catch (error) {
  console.error(`\n💥 Error: ${error.message}\n`)
} finally {
  await browser.close()
}
