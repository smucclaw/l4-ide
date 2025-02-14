/** @type {import('tailwindcss').Config} */

export default {
  // Needs to be important to override the default styles
  important: true,
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
