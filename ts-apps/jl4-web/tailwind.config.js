import defaultTheme from 'tailwindcss/defaultTheme'
import tailwindcssAnimate from 'tailwindcss-animate'

const config = {
  content: ['./src/**/*.{html,js,svelte,ts}'],
  theme: {
    container: {
      center: true,
      padding: '2rem',
      screens: {
        '2xl': '1400px',
      },
    },
    extend: {
      colors: {
        border: 'var(--border) / <alpha-value>',
        input: 'var(--input) / <alpha-value>',
        ring: 'var(--ring) / <alpha-value>',
        background: 'var(--background) / <alpha-value>',
        foreground: 'var(--foreground) / <alpha-value>',
        primary: {
          DEFAULT: 'var(--primary) / <alpha-value>',
          foreground: 'var(--primary-foreground) / <alpha-value>',
        },
        secondary: {
          DEFAULT: 'var(--secondary) / <alpha-value>',
          foreground: 'var(--secondary-foreground) / <alpha-value>',
        },
        destructive: {
          DEFAULT: 'var(--destructive) / <alpha-value>',
          foreground: 'var(--destructive-foreground) / <alpha-value>',
        },
        muted: {
          DEFAULT: 'var(--muted) / <alpha-value>',
          foreground: 'var(--muted-foreground) / <alpha-value>',
        },
        accent: {
          DEFAULT: 'var(--accent) / <alpha-value>',
          foreground: 'var(--accent-foreground) / <alpha-value>',
        },
        popover: {
          DEFAULT: 'var(--popover) / <alpha-value>',
          foreground: 'var(--popover-foreground) / <alpha-value>',
        },
        card: {
          DEFAULT: 'var(--card) / <alpha-value>',
          foreground: 'var(--card-foreground) / <alpha-value>',
        },
        sidebar: {
          DEFAULT: 'var(--sidebar-background)',
          foreground: 'var(--sidebar-foreground)',
          primary: 'var(--sidebar-primary)',
          'primary-foreground': 'var(--sidebar-primary-foreground)',
          accent: 'var(--sidebar-accent)',
          'accent-foreground': 'var(--sidebar-accent-foreground)',
          border: 'var(--sidebar-border)',
          ring: 'var(--sidebar-ring)',
        },
      },
      borderRadius: {
        xl: 'calc(var(--radius) + 4px)',
        lg: 'var(--radius)',
        md: 'calc(var(--radius) - 2px)',
        sm: 'calc(var(--radius) - 4px)',
      },
      fontFamily: {
        sans: defaultTheme.fontFamily.sans,
      },
      keyframes: {
        'accordion-down': {
          from: { height: '0' },
          to: { height: 'var(--bits-accordion-content-height)' },
        },
        'accordion-up': {
          from: { height: 'var(--bits-accordion-content-height)' },
          to: { height: '0' },
        },
        'caret-blink': {
          '0%,70%,100%': { opacity: '1' },
          '20%,50%': { opacity: '0' },
        },
      },
      animation: {
        'accordion-down': 'accordion-down 0.2s ease-out',
        'accordion-up': 'accordion-up 0.2s ease-out',
        'caret-blink': 'caret-blink 1.25s ease-out infinite',
      },
    },
  },
  plugins: [tailwindcssAnimate],
}

export default config
