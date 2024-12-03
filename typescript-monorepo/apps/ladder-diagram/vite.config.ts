import { sveltekit } from '@sveltejs/kit/vite'
import { defineConfig } from 'vite'
import path from 'path'

export default defineConfig({
  plugins: [sveltekit()],
  resolve: {
    alias: {
      'ladder-diagram': path.resolve(
        __dirname,
        'node_modules/ladder-diagram/js'
      ),
    },
  },
})
