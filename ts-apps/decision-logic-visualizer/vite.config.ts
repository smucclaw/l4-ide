import { sveltekit } from '@sveltejs/kit/vite'
import tailwindcss from '@tailwindcss/vite'
import { defineConfig } from 'vitest/config'

export default defineConfig({
  plugins: [sveltekit(), tailwindcss()],
  ssr: {
    noExternal: ['@xyflow/svelte'],
  },
  test: {
    include: ['src/**/*.test.ts'],
  },
})
