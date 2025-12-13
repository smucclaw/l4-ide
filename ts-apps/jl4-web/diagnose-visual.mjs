#!/usr/bin/env node
import { chromium } from 'playwright'

console.log('🔍 Checking LSP visual decorations...\n')

const browser = await chromium.launch()
const context = await browser.newContext()
const page = await context.newPage()

// Track important console messages
page.on('console', (msg) => {
  const text = msg.text()
  if (
    text.includes('semantic') ||
    text.includes('token') ||
    text.includes('highlight') ||
    text.includes('decoration')
  ) {
    console.log(`[CONSOLE] ${text}`)
  }
})

console.log('📄 Loading page...\n')
await page.goto('http://localhost:5173/', { waitUntil: 'networkidle' })

// Wait for editor to load
console.log('⏳ Waiting for editor initialization (8 seconds)...\n')
await page.waitForTimeout(8000)

// Take a screenshot
await page.screenshot({ path: '/tmp/l4-ide-screenshot.png', fullPage: true })
console.log('📸 Screenshot saved to /tmp/l4-ide-screenshot.png\n')

// Check for Monaco editor
const editorCount = await page.locator('.monaco-editor').count()
console.log(`✓ Monaco editors found: ${editorCount}`)

// Check for specific Monaco elements that indicate rendering
const hasLineNumbers = (await page.locator('.line-numbers').count()) > 0
const hasLines = (await page.locator('.view-line').count()) > 0
const hasTokens =
  (await page.locator('.mtk1, .mtk2, .mtk3, .mtk4, .mtk5').count()) > 0

console.log(`✓ Line numbers visible: ${hasLineNumbers}`)
console.log(`✓ Editor lines rendered: ${hasLines}`)
console.log(`✓ Syntax tokens present: ${hasTokens}`)

// Check for error decorations (squiggles)
const errorDecorations = await page
  .locator('.squiggly-error, .squiggly-warning, .squiggly-info')
  .count()
console.log(`✓ Error decorations: ${errorDecorations}`)

// Check editor content
const editorText = await page
  .locator('.monaco-editor .view-line')
  .first()
  .textContent()
console.log(`\n📝 First line of editor: "${editorText?.slice(0, 50)}..."`)

// Check if semantic highlighting is enabled
const semanticHighlightEnabled = await page.evaluate(() => {
  const editor = document.querySelector('.monaco-editor')
  if (!editor) return false

  // Check for semantic token class names
  const hasSemanticTokens = !!document.querySelector('[class*="mtks"]')
  return hasSemanticTokens
})
console.log(`✓ Semantic highlighting tokens: ${semanticHighlightEnabled}`)

console.log('\n✅ Visual diagnostic complete')
console.log('View screenshot: open /tmp/l4-ide-screenshot.png\n')

await browser.close()
