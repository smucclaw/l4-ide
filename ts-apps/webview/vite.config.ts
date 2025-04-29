import { sveltekit } from '@sveltejs/kit/vite'
import { defineConfig } from 'vite'
import tailwindcss from '@tailwindcss/vite'

export default defineConfig({
  plugins: [sveltekit(), tailwindcss()],
  build: {
    rollupOptions: {
      external: ['vscode-webview'],
    },
    // While we're still in alpha: Make it easier to debug
    minify: false,
    sourcemap: true,
  },
})
