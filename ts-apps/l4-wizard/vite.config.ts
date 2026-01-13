import { sveltekit } from '@sveltejs/kit/vite'
import tailwindcss from '@tailwindcss/vite'
import { defineConfig } from 'vite'

export default defineConfig({
  plugins: [tailwindcss(), sveltekit()],
  server: {
    host: true, // Listen on all addresses including LAN
    port: 5174, // Different port from jl4-web (5173)
    allowedHosts: true as unknown as undefined, // Vite 6.0.6+ security feature
  },
})
