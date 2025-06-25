import {
  readdirSync,
  copyFileSync,
  mkdirSync,
  existsSync,
  writeFileSync,
} from 'fs'
import { dirname, resolve, join } from 'path'
import { fileURLToPath } from 'url'

// Debug flag - set to true to see detailed path resolution information
// Useful for troubleshooting path-related issues in the Nix build environment
const DEBUG = false

const __filename = fileURLToPath(import.meta.url)
const __dirname = dirname(__filename)

const sourceDir = resolve(__dirname, '../../../jl4/examples/legal')
const targetDir = resolve(__dirname, '../src/generated')

if (DEBUG) {
  console.log('Debug: Current directory:', process.cwd())
  console.log('Debug: __dirname:', __dirname)
  console.log('Debug: Attempting to resolve source directory:', sourceDir)
  console.log('Debug: Source directory exists:', existsSync(sourceDir))
}

// Ensure target directory exists
if (!existsSync(targetDir)) {
  if (DEBUG) console.log('Debug: Creating target directory:', targetDir)
  mkdirSync(targetDir, { recursive: true })
}

const emptyFileName = 'empty.l4'
const emptyFilePath = join(targetDir, emptyFileName)
writeFileSync(emptyFilePath, '')

// Copy all .l4 files
const files = readdirSync(sourceDir).filter((file) => file.endsWith('.l4'))

if (files.length === 0) {
  throw new Error('No .l4 files found in jl4/examples/legal/')
}

files.forEach((file) => {
  const sourcePath = join(sourceDir, file)
  const targetPath = join(targetDir, file)
  if (DEBUG)
    console.log(`Debug: Copying ${file} from ${sourcePath} to ${targetPath}`)
  copyFileSync(sourcePath, targetPath)
})

console.log(
  `✓ Copied ${files.length} example files to src/examples/ (and the empty file)`
)
