// Standalone SPA: no server-side rendering, all logic runs in the browser.
// The root (/) is prerendered to index.html as the app shell; dynamic routes
// like /[org]/[deployment] are served via the 404.html fallback on GitHub Pages.
export const ssr = false
export const prerender = true
