import { sveltekit } from '@sveltejs/kit/vite'
import tailwindcss from '@tailwindcss/vite'
import { defineConfig } from 'vite'
import { execSync } from 'child_process'

// Get git commit hash at build time (with fallback for sandboxed builds)
let commitHash = 'unknown'
try {
  commitHash = execSync('git rev-parse --short HEAD').toString().trim()
} catch {
  // git not available in sandboxed Nix builds
  commitHash = process.env.GIT_COMMIT_HASH ?? 'dev'
}

export default defineConfig({
  plugins: [tailwindcss(), sveltekit()],
  define: {
    __COMMIT_HASH__: JSON.stringify(commitHash),
  },
  server: {
    host: true, // Listen on all addresses including LAN
    port: 5174, // Different port from jl4-web (5173)
    allowedHosts: true as unknown as undefined, // Vite 6.0.6+ security feature
  },
})
