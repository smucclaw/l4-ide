#!/usr/bin/env node

import { execSync } from 'child_process';
import { readFileSync, writeFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const projectRoot = join(__dirname, '..');

console.log('🚀 Preparing L4 VSCode Extension for Marketplace Publication');
console.log('==========================================================\n');

// Check if vsce is installed
try {
  execSync('npx vsce --version', { stdio: 'pipe' });
} catch (error) {
  console.error('❌ vsce is not installed. Installing...');
  execSync('npm install -g @vscode/vsce', { stdio: 'inherit' });
}

// Build the extension
console.log('📦 Building extension...');
try {
  execSync('npm run build', { cwd: projectRoot, stdio: 'inherit' });
  console.log('✅ Build completed successfully');
} catch (error) {
  console.error('❌ Build failed:', error.message);
  process.exit(1);
}

// Package the extension
console.log('\n📦 Packaging extension...');
try {
  execSync('npm run package', { cwd: projectRoot, stdio: 'inherit' });
  console.log('✅ Package created successfully');
} catch (error) {
  console.error('❌ Packaging failed:', error.message);
  process.exit(1);
}

// Check package.json for required fields
console.log('\n🔍 Validating package.json...');
const packageJson = JSON.parse(readFileSync(join(projectRoot, 'package.json'), 'utf8'));

const requiredFields = [
  'name', 'displayName', 'description', 'version', 'publisher',
  'engines', 'categories', 'keywords', 'icon'
];

const missingFields = requiredFields.filter(field => !packageJson[field]);
if (missingFields.length > 0) {
  console.error('❌ Missing required fields in package.json:', missingFields);
  process.exit(1);
}

console.log('✅ Package.json validation passed');

// Check if icon exists
const iconPath = join(projectRoot, 'static', 'icon.png');
try {
  const fs = await import('fs');
  fs.accessSync(iconPath);
  console.log('✅ Icon file exists');
} catch (error) {
  console.warn('⚠️  Warning: Icon file not found. Please add a 128x128 PNG icon at static/icon.png');
}

console.log('\n🎉 Extension is ready for publication!');
console.log('\nNext steps:');
console.log('1. Test the extension locally:');
console.log('   - Open VS Code');
console.log('   - Go to Extensions view');
console.log('   - Click "..." and select "Install from VSIX..."');
console.log('   - Select the generated .vsix file');
console.log('\n2. Publish to marketplace:');
console.log('   - Create a Personal Access Token at https://dev.azure.com');
console.log('   - Run: vsce publish');
console.log('   - Or run: vsce publish --packagePath jl4-vscode-1.0.0.vsix');
console.log('\n3. Update version number for future releases');
