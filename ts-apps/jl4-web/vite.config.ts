import tailwindcss from '@tailwindcss/vite'
import { sveltekit } from '@sveltejs/kit/vite'
import { defineConfig } from 'vite'

export default defineConfig({
  plugins: [tailwindcss(), sveltekit()],
  worker: {
    format: 'es',
  },
  server: {
    host: '0.0.0.0',
    allowedHosts: true as unknown as undefined, // Vite 6.0.6+ security feature
  },
})
