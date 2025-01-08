export default {
  printWidth: 80,
  tabWidth: 2,
  useTabs: false,
  bracketSpacing: true,
  singleQuote: true,
  trailingComma: 'es5',
  semi: false,
  plugins: ['prettier-plugin-svelte'],
  overrides: [
    {
      files: '*.svelte',
      options: {
        parser: 'svelte',
      },
    },
  ],
}
