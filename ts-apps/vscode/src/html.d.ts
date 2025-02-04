// So that we can import .html files as strings, when creating the webview panel
declare module '*.html' {
  const content: string
  export default content
}
