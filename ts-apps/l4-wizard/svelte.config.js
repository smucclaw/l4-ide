import adapter from '@sveltejs/adapter-static'
import { vitePreprocess } from '@sveltejs/vite-plugin-svelte'

/** @type {import('@sveltejs/kit').Config} */
const config = {
  preprocess: vitePreprocess(),
  kit: {
    adapter: adapter({
      pages: 'build',
      assets: 'build',
      fallback: 'index.html',
      precompress: false,
      strict: true,
    }),
    // Allow serving from a subdirectory (e.g., /wizard/) via VITE_BASE_PATH
    paths: {
      base: process.env.VITE_BASE_PATH || '',
    },
  },
}

export default config
