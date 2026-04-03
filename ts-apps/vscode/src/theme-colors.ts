/**
 * Extracts syntax token colors from the active VS Code color theme.
 *
 * VS Code doesn't expose token colors as CSS variables in webviews,
 * so we read them from the theme JSON file and inject them as
 * --l4-tok-* CSS custom properties.
 */

import * as vscode from 'vscode'
import * as path from 'path'
import * as fs from 'fs'

export interface TokenColors {
  keyword: string
  directive: string
  comment: string
  string: string
  number: string
  variable: string
  operator: string
  identifier: string
}

/**
 * TextMate scope → token type mapping.
 * Order within each token type matters: first match wins.
 */
const SCOPE_MAP: [keyof TokenColors, string[]][] = [
  // L4 keywords (AND, OR, IF, …) are control-flow words — map to
  // the theme's keyword.control color (purple in Default Dark Modern).
  ['keyword', ['keyword.control', 'keyword']],
  // L4 directives (#EVAL, #ASSERT, …) are preprocessor markers — map to
  // the theme's meta.preprocessor color (blue in Default Dark Modern).
  ['directive', ['meta.preprocessor', 'keyword.other.directive']],
  ['comment', ['comment.line', 'comment.block', 'comment']],
  ['string', ['string.quoted', 'string']],
  ['number', ['constant.numeric', 'constant']],
  ['variable', ['variable.other', 'variable']],
  ['operator', ['keyword.operator']],
  [
    'identifier',
    ['entity.name.type', 'entity.name.class', 'support.type', 'entity.name'],
  ],
]

const DARK_DEFAULTS: TokenColors = {
  keyword: '#c586c0',
  directive: '#569cd6',
  comment: '#6a9955',
  string: '#ce9178',
  number: '#b5cea8',
  variable: '#9cdcfe',
  operator: '#d4d4d4',
  identifier: '#4ec9b0',
}

const LIGHT_DEFAULTS: TokenColors = {
  keyword: '#af00db',
  directive: '#0000ff',
  comment: '#008000',
  string: '#a31515',
  number: '#098658',
  variable: '#001080',
  operator: '#000000',
  identifier: '#267f99',
}

const HC_DEFAULTS: TokenColors = {
  ...DARK_DEFAULTS,
}

function defaultsForKind(kind: vscode.ColorThemeKind): TokenColors {
  switch (kind) {
    case vscode.ColorThemeKind.Light:
    case vscode.ColorThemeKind.HighContrastLight:
      return { ...LIGHT_DEFAULTS }
    case vscode.ColorThemeKind.HighContrast:
      return { ...HC_DEFAULTS }
    default:
      return { ...DARK_DEFAULTS }
  }
}

interface ThemeTokenColorRule {
  scope?: string | string[]
  settings?: { foreground?: string; fontStyle?: string }
}

/**
 * Try to read the active theme's JSON file and extract tokenColors.
 */
function readThemeTokenColors(): ThemeTokenColorRule[] {
  const themeId = vscode.workspace
    .getConfiguration('workbench')
    .get<string>('colorTheme')
  if (!themeId) return []

  for (const ext of vscode.extensions.all) {
    const themes = ext.packageJSON?.contributes?.themes as
      | Array<{ id?: string; label?: string; path?: string }>
      | undefined
    if (!themes) continue

    const match = themes.find((t) => t.id === themeId || t.label === themeId)
    if (!match?.path) continue

    const themePath = path.join(ext.extensionPath, match.path)
    try {
      const raw = fs.readFileSync(themePath, 'utf-8')
      // Handle JSON with comments (JSONC) by stripping them
      const stripped = raw
        .replace(/\/\/.*$/gm, '')
        .replace(/\/\*[\s\S]*?\*\//g, '')
      const parsed = JSON.parse(stripped) as {
        tokenColors?: ThemeTokenColorRule[]
        include?: string
      }

      let rules = parsed.tokenColors ?? []

      // If the theme includes a base theme, read that too
      if (parsed.include) {
        const basePath = path.join(path.dirname(themePath), parsed.include)
        try {
          const baseRaw = fs.readFileSync(basePath, 'utf-8')
          const baseStripped = baseRaw
            .replace(/\/\/.*$/gm, '')
            .replace(/\/\*[\s\S]*?\*\//g, '')
          const baseParsed = JSON.parse(baseStripped) as {
            tokenColors?: ThemeTokenColorRule[]
          }
          // Base rules first, then theme-specific rules override
          rules = [...(baseParsed.tokenColors ?? []), ...rules]
        } catch {
          // Ignore base theme read errors
        }
      }

      return rules
    } catch {
      // Ignore parse errors, fall through to defaults
    }
  }
  return []
}

/**
 * Find the foreground color for a given TextMate scope from the token rules.
 * Longer/more-specific scope matches take priority.
 */
function findColor(
  rules: ThemeTokenColorRule[],
  scopes: string[]
): string | undefined {
  let bestColor: string | undefined
  let bestSpecificity = -1

  for (const rule of rules) {
    if (!rule.settings?.foreground) continue
    const ruleScopes = Array.isArray(rule.scope)
      ? rule.scope
      : rule.scope
        ? [rule.scope]
        : []

    for (const ruleScope of ruleScopes) {
      for (const targetScope of scopes) {
        if (
          targetScope === ruleScope ||
          targetScope.startsWith(ruleScope + '.') ||
          ruleScope.startsWith(targetScope + '.') ||
          ruleScope.startsWith(targetScope)
        ) {
          // More specific (longer) rule scope wins
          const specificity = ruleScope.length
          if (specificity > bestSpecificity) {
            bestSpecificity = specificity
            bestColor = rule.settings.foreground
          }
        }
      }
    }
  }
  return bestColor
}

/**
 * Get token colors from the active VS Code theme.
 * Reads the theme file and falls back to sensible defaults.
 */
export function getTokenColors(): TokenColors {
  const kind = vscode.window.activeColorTheme.kind
  const defaults = defaultsForKind(kind)
  const rules = readThemeTokenColors()

  if (rules.length === 0) return defaults

  const colors: TokenColors = { ...defaults }
  for (const [tokenType, scopes] of SCOPE_MAP) {
    const found = findColor(rules, scopes)
    if (found) {
      colors[tokenType] = found
    }
  }

  // If directive color wasn't found via meta.preprocessor, try plain keyword scope
  if (!findColor(rules, SCOPE_MAP.find(([k]) => k === 'directive')![1])) {
    const kwFallback = findColor(rules, ['keyword'])
    if (kwFallback) colors.directive = kwFallback
  }

  return colors
}

/**
 * Generate a CSS <style> block with --l4-tok-* custom properties.
 */
export function tokenColorsToCSS(colors: TokenColors): string {
  return `<style id="l4-token-colors">
:root {
  --l4-tok-keyword: ${colors.keyword};
  --l4-tok-directive: ${colors.directive};
  --l4-tok-comment: ${colors.comment};
  --l4-tok-string: ${colors.string};
  --l4-tok-number: ${colors.number};
  --l4-tok-variable: ${colors.variable};
  --l4-tok-operator: ${colors.operator};
  --l4-tok-identifier: ${colors.identifier};
}
</style>`
}
