import { sveltekit } from '@sveltejs/kit/vite'
import { defineConfig } from 'vite'
import path from 'path'
import tailwindcss from '@tailwindcss/vite'

export default defineConfig({
  plugins: [sveltekit(), tailwindcss()],
  resolve: {
    // alias: {
    //   '@repo/decision-logic-visualizer': path.resolve(
    //     '../decision-logic-visualizer'
    //   ),
    // },
  },
  build: {
    rollupOptions: {
      external: ['vscode-webview'],
    },
  },
})
