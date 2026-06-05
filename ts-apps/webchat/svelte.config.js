import adapter from '@sveltejs/adapter-static'
import { vitePreprocess } from '@sveltejs/vite-plugin-svelte'

/** @type {import('@sveltejs/kit').Config} */
const config = {
  preprocess: vitePreprocess(),

  kit: {
    // Served at the root of chat.legalese.cloud (custom domain), so no base path.
    adapter: adapter({
      pages: 'build',
      assets: 'build',
      // GitHub Pages serves 404.html for any unmatched path. Emitting the SPA
      // shell there lets deep links like /{org}/{deployment} boot the app and
      // route client-side. The root (/) is prerendered to index.html (200).
      fallback: '404.html',
      precompress: false,
      strict: false,
    }),
  },
}

export default config
