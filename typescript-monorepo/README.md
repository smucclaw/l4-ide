# README

## Key scripts

- To lint: `turbo lint`
- To format with Prettier: `pnpm format` (TODO: May want to make this a Turbo task instead)

### Notes on configs setup

* There are shared `eslint` and `prettier` configs in `./shared`.
* ESLint: We're using the flat config file format.
* Prettier: Not sure yet what should be done in the sub-package/app package.json vs. the top-level package.json. But for now, you can `pnpm format` from `mattwaddington/typescript-monorepo` as well as from the vscode extension directory.
* Haven't tried to add Prettier/ESLint extensions to `.vscode/settings.json` yet,
because my experience with VSCode Prettier extensions hasn't always been positive.
Will try to think about what a good local dev setup might be, 
wrt formatting --- perhaps we could use pre-commit / commit hooks?


## What's inside?

This Turborepo includes the following packages/apps:

### Apps and Packages

- `@repo/ui`: a stub component library
- `@repo/eslint-config`: `eslint` configurations (includes `eslint-config-next` and `eslint-config-prettier`)
- `@repo/prettier-config`
- `@repo/typescript-config`: `tsconfig.json`s used throughout the monorepo. [TODO: Might not be using this right now.]

### Build

To build all apps and packages, run the following command:

```
cd my-turborepo
pnpm build
```

### Develop

To develop all apps and packages, run the following command:

```
cd my-turborepo
pnpm dev
```

### Remote Caching

Turborepo can use a technique known as [Remote Caching](https://turbo.build/repo/docs/core-concepts/remote-caching) to share cache artifacts across machines, enabling you to share build caches with your team and CI/CD pipelines.

By default, Turborepo will cache locally. To enable Remote Caching you will need an account with Vercel. If you don't have an account you can [create one](https://vercel.com/signup), then enter the following commands:

```
cd my-turborepo
npx turbo login
```

This will authenticate the Turborepo CLI with your [Vercel account](https://vercel.com/docs/concepts/personal-accounts/overview).

Next, you can link your Turborepo to your Remote Cache by running the following command from the root of your Turborepo:

```
npx turbo link
```

## Useful Links

Learn more about the power of Turborepo:

- [Tasks](https://turbo.build/repo/docs/core-concepts/monorepos/running-tasks)
- [Caching](https://turbo.build/repo/docs/core-concepts/caching)
- [Remote Caching](https://turbo.build/repo/docs/core-concepts/remote-caching)
- [Filtering](https://turbo.build/repo/docs/core-concepts/monorepos/filtering)
- [Configuration Options](https://turbo.build/repo/docs/reference/configuration)
- [CLI Usage](https://turbo.build/repo/docs/reference/command-line-reference)
