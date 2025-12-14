#!/usr/bin/env node
import { chromium } from 'playwright'

const url = process.env.URL || 'http://localhost:5173/'
const timeout = parseInt(process.env.TIMEOUT || '10000')

console.log(`\n🔍 Diagnosing frontend at: ${url}\n`)

const browser = await chromium.launch({ headless: true })
const context = await browser.newContext()
const page = await context.newPage()

// Capture console messages
const consoleMessages = []
page.on('console', (msg) => {
  const type = msg.type()
  const text = msg.text()
  consoleMessages.push({ type, text })

  const emoji =
    {
      error: '❌',
      warning: '⚠️ ',
      info: 'ℹ️ ',
      log: '📝',
    }[type] || '  '

  console.log(`${emoji} [console.${type}] ${text}`)
})

// Capture page errors
const pageErrors = []
page.on('pageerror', (error) => {
  pageErrors.push(error.message)
  console.log(`💥 [pageerror] ${error.message}`)
})

// Capture network failures
const networkErrors = []
page.on('requestfailed', (request) => {
  const failure = request.failure()
  networkErrors.push({
    url: request.url(),
    error: failure ? failure.errorText : 'Unknown error',
  })
  console.log(`🌐 [network] Failed: ${request.url()}`)
  if (failure) console.log(`   Error: ${failure.errorText}`)
})

// Capture response errors (4xx, 5xx)
const responseErrors = []
page.on('response', (response) => {
  const status = response.status()
  if (status >= 400) {
    responseErrors.push({
      url: response.url(),
      status,
      statusText: response.statusText(),
    })
    console.log(
      `🌐 [response] ${status} ${response.statusText()}: ${response.url()}`
    )
  }
})

try {
  // Navigate to the page
  await page.goto(url, { waitUntil: 'networkidle', timeout })

  // Wait a bit for any lazy-loaded content
  await page.waitForTimeout(2000)

  // Check if page has any visible content
  const bodyText = await page.textContent('body')
  const hasContent = bodyText && bodyText.trim().length > 0

  // Take a screenshot
  await page.screenshot({ path: 'screenshot.png', fullPage: true })

  console.log('\n📊 Summary:')
  console.log(`   Console messages: ${consoleMessages.length}`)
  console.log(`   Page errors: ${pageErrors.length}`)
  console.log(`   Network failures: ${networkErrors.length}`)
  console.log(`   Response errors: ${responseErrors.length}`)
  console.log(`   Page has content: ${hasContent}`)
  console.log(`   Screenshot saved: screenshot.png\n`)

  // Check for specific elements
  console.log('🔍 Checking for key UI elements...')

  const checks = [
    { selector: 'h1', name: 'Page heading' },
    { selector: '[data-testid="editor"]', name: 'Code editor' },
    { selector: 'button', name: 'Any buttons' },
    { selector: '.sidebar', name: 'Sidebar' },
    { selector: 'nav', name: 'Navigation' },
  ]

  for (const check of checks) {
    const exists = (await page.locator(check.selector).count()) > 0
    const emoji = exists ? '✅' : '❌'
    console.log(`   ${emoji} ${check.name} (${check.selector})`)
  }

  // Get the page title
  const title = await page.title()
  console.log(`\n📄 Page title: "${title}"`)
} catch (error) {
  console.error(`\n💥 Fatal error: ${error.message}\n`)
  process.exit(1)
} finally {
  await browser.close()
}
