// Deployment chat is bound at runtime from the URL params, so it cannot be
// prerendered. It is served via the 404.html SPA fallback on GitHub Pages.
export const prerender = false
