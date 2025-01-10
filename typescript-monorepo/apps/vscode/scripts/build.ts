/************************************************************************

  Adapted from
  https://github.com/Daydreamer-riri/vscode-ext-iconfont-reminder/blob/f8306bcc216ceb3c7919d55df7dbe878cf1e39e4/scripts/build.ts

**************************************************************************/

import { execSync } from 'node:child_process'
import * as fs from 'fs-extra'

async function build() {
  // Clean
  await fs.remove('./dist')

  // Bundle with tsup
  execSync('tsup src/extension.ts --format cjs --external vscode --no-shims', {
    stdio: 'inherit',
  })

  const files = ['README.md', 'media']

  for (const f of files) {
    await fs.copy(`./${f}`, `./dist/${f}`)
  }

  // Adapt package.json for the built VSCode extension
  const packageJson = await fs.readJSON('./package.json')
  delete packageJson.scripts
  delete packageJson.devDependencies
  packageJson.main = 'extension.js'
  await fs.writeJSON('./dist/package.json', packageJson)
}

build()
