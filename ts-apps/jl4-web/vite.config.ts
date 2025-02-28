import { sveltekit } from "@sveltejs/kit/vite";
import { defineConfig } from "vite";

export default defineConfig({
  plugins: [sveltekit()],
  worker: {
    format: "es",
  },
  // ssr: {
  //   noExternal: ["@codingame/monaco-vscode-api"]
  // },
  optimizeDeps: {
    exclude: ["@codingame/monaco-vscode-api", "monaco-editor"]
  }
});
