import adapter from '@sveltejs/adapter-static'
import { vitePreprocess } from '@sveltejs/vite-plugin-svelte'
// import tsconfigPaths from 'vite-tsconfig-paths'

/** @type {import('@sveltejs/kit').Config} */
const config = {
  // Consult https://svelte.dev/docs/kit/integrations
  // for more information about preprocessors
  extensions: ['.svelte'],
  preprocess: [vitePreprocess()],
  // plugins: [tsconfigPaths()],

  kit: {
    adapter: adapter({
      // default options are shown. On some platforms
      // these options are set automatically — see below
      pages: 'build',
      assets: 'build',
      fallback: './src/error.html',
      precompress: false,
      strict: true,
    }),
    serviceWorker: {
      register: false,
    },
  },
}

export default config
