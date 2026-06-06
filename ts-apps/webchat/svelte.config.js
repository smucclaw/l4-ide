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
    // Content-Security-Policy — mirrors the protections on legalese.com/console,
    // scoped to the domains this app actually talks to:
    //   - self                → chat.legalese.cloud (shell, JS, CSS)
    //   - legalese.cloud + *. → auth (apex /auth/session), api. (deployment
    //                           metadata + function schemas), ai. (chat SSE)
    //   - workoscdn / google  → WorkOS/Google account profile pictures
    //
    // `mode: 'hash'` makes SvelteKit hash its inline hydration-bootstrap script
    // and append that hash to script-src, so scripts get a strict policy with
    // NO 'unsafe-inline'. style-src is also strict ('self' only): the app ships
    // no inline `style=""` attributes (the body wrapper uses an `.app-shell`
    // class and dynamic widths use Svelte `style:` directives, applied via the
    // CSSOM, which CSP doesn't govern) and the prod build extracts all CSS to
    // external files. SvelteKit emits the policy as a <meta> tag on the
    // prerendered/static pages — the only option on GitHub Pages, which can't
    // set response headers.
    csp: {
      mode: 'hash',
      directives: {
        'default-src': ['self'],
        'script-src': ['self'],
        'style-src': ['self'],
        // `data:` is needed because Vite inlines KaTeX's small woff2 fonts
        // (math rendering in markdown) as data: URIs under its asset-inline
        // limit; the rest load from self. A data: font is inert (no script
        // execution), so this is a safe relaxation.
        'font-src': ['self', 'data:'],
        'img-src': [
          'self',
          'https://workoscdn.com',
          'https://*.googleusercontent.com',
        ],
        'connect-src': [
          'self',
          'https://legalese.cloud',
          'https://*.legalese.cloud',
        ],
        'frame-src': ['none'],
        'object-src': ['none'],
        'base-uri': ['self'],
      },
    },
  },
}

export default config
