// Disable SSR so that, when running in dev mode,
// sf nodes won't try to render and measure themselves during SSR / in a non-browser environment.
// (That results in weird behavior like a bool var node that gets measured as 1pixel x 1pixel.)
export const ssr = false
export const trailingSlash = 'always'
