import { sveltekit } from '@sveltejs/kit/vite'
import tailwindcss from '@tailwindcss/vite'
import { defineConfig } from 'vitest/config'

export default defineConfig({
  plugins: [tailwindcss(), sveltekit()],
  ssr: {
    noExternal: ['@xyflow/svelte', '@xyflow/system'],
  },
  test: {
    include: ['src/**/*.test.ts'],
  },
})
