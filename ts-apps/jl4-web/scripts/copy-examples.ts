import { readdirSync, copyFileSync, mkdirSync, existsSync } from 'fs';
import { dirname, resolve, join } from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

const sourceDir = resolve(__dirname, '../../../jl4/examples/legal');
const targetDir = resolve(__dirname, '../static/examples');

// Ensure target directory exists
if (!existsSync(targetDir)) {
  mkdirSync(targetDir, { recursive: true });
}

// Copy all .l4 files
const files = readdirSync(sourceDir).filter(file => file.endsWith('.l4'));

if (files.length === 0) {
  throw new Error('No .l4 files found in jl4/examples/legal/');
}

files.forEach(file => {
  const sourcePath = join(sourceDir, file);
  const targetPath = join(targetDir, file);
  copyFileSync(sourcePath, targetPath);
});

console.log(`✓ Copied ${files.length} example files to static/examples/`); 