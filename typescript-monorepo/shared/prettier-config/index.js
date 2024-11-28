export default {
  printWidth: 80,
  tabWidth: 2,
  useTabs: false,
  bracketSpacing: true,
  singleQuote: true,
  trailingComma: 'es5',
  semiColons: false,
  plugins: [
    'prettier-plugin-svelte'
  ],
  overrides: [
		{
			"files": "*.svelte",
			"options": {
				"parser": "svelte"
			}
		}
  ]
}