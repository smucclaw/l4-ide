import { sveltekit } from '@sveltejs/kit/vite'
import autoprefixer from 'autoprefixer'
import { defineConfig } from 'vitest/config'

export default defineConfig({
  plugins: [sveltekit()],
  ssr: {
    noExternal: ['@xyflow/svelte'],
  },
  css: {
    postcss: {
      plugins: [autoprefixer()],
    },
  },
  test: {
    include: ['src/**/*.test.ts'],
  },
})
