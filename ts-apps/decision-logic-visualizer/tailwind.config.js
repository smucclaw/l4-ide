/** @type {import('tailwindcss').Config} */

export default {
  content: ['./src/**/*.{html,js,svelte,ts}'],
  theme: {
    colors: {
      transparent: 'transparent',
      primary: 'var(--color-primary)',
      secondary: 'var(--color-secondary)',
    },
    extend: {},
  },
  plugins: ['@tailwindcss/postcss'],
}
