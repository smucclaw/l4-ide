# README

## Installation for contributors

Install node.js (>= 18) if you don't already have it.

Then do the following from `typescript-monorepo`.

To ensure that we're using the same version of the same package manager

- Enable corepack with `corepack enable`.

- Then `corepack install` to install the package manager used by this monorepo.

Then `pnpm install`, again from `typescript-monorepo`.

Finally, you'll want to install `turbo` _both_ globally and locally.

- To install it _globally_: `pnpm install turbo --global` or `npm install turbo --global`.

(See https://turbo.build/repo/docs/getting-started/installation if you need more help with turbo installation.)

Once the dependencies have been installed,
the TS projects can be built by running `turbo build`
from the `typescript-monorepo` dir.

## Key scripts

- To lint: `turbo lint`
- To format with Prettier: `pnpm format` (TODO: May want to make this a Turbo task instead)
  - There is also a VSCode task for formatting the monorepo.
- To develop all apps and packages: `turbo dev`

### Further notes on setup / build process / configs for developers of the TS monorepo

#### VSCode extension build related

What follows are notes on the bundling implementation --- ignore this
if you are an end user as opposed to contributor to the VSCode extension.

As of Jan 11 2025, we bundle the dependencies for the VSCode extension
and package the extension with `pnpm dlx vsce package --no-dependencies`.

The initial motivation for this had to do with how `vsce` cannot be used naively with `pnpm`;
in particular, vsce+pnpm (when you don't do the bundling yourself) cannot
correctly resolve `node_modules` and include them in the VSIX file.

That said, there's also independent reasons to do this sort of bundling; e.g.:

- it allows you to use ESM in your extension-related code (but still compile to CommonJS for VSCode extensions)
- you'll need to do bundling if you want to target the browser
- in general it's probably better to control the bundling process yourself, instead of letting `vsce` guess at how you want to bundle your dependencies

That is, even though the bundling was initially prompted by `vsce`
not supporting `pnpm` (when used in a naive way),
I wouldn't call this a 'workaround' --- it's something
we have independent reason to do anyway.

#### Why use `tsup` for bundling the VSCode extension dependencies?

You might wonder why `tsup` is being used for bundling the VSCode extension dependencies,
given that we also are using `vite` when bundling the `decision-logic-visualizer`.

The answer is:

- Vite's CJS Node API is being deprecated (https://vite.dev/guide/troubleshooting.html#vite-cjs-node-api-deprecated),
  which makes it non-ideal for bundling VSCode extensions (they require CJS)
- It may be possible to replace `vite` with `tsup` as well, but that will require some looking into --- the reason why `vite` came into the picture in the first place is because it's what `sveltekit` uses.

#### Prettier and ESLint

- There are shared `eslint` and `prettier` configs in `./shared`.
- ESLint: We're using the flat config file format.
- Prettier: Not sure yet what should be done in the sub-package/app package.json vs. the top-level package.json. But for now, you can `pnpm format` from `mattwaddington/typescript-monorepo` as well as from the vscode extension directory.
- Haven't tried to add Prettier/ESLint extensions to `.vscode/settings.json` yet,
  because my experience with VSCode Prettier extensions hasn't always been positive.
  But note that there's a `Format TS Monorepo` VSCode task that can be activated from the Command Palette.
  And perhaps we could use pre-commit / commit hooks?

## What's inside?

This Turborepo includes the following packages/apps:

### Apps and Packages

- `@repo/ui`: a stub component library
- `@repo/eslint-config`: `eslint` configurations (includes `eslint-config-next` and `eslint-config-prettier`)
- `@repo/prettier-config`
- `@repo/typescript-config`: `tsconfig.json`s used throughout the monorepo. [TODO: Might not be using this right now.]

### Remote Caching

Turborepo can use a technique known as [Remote Caching](https://turbo.build/repo/docs/core-concepts/remote-caching) to share cache artifacts across machines, enabling you to share build caches with your team and CI/CD pipelines.

By default, Turborepo will cache locally. To enable Remote Caching you will need an account with Vercel.

## Useful Links

Learn more about the power of Turborepo:

- [Tasks](https://turbo.build/repo/docs/core-concepts/monorepos/running-tasks)
- [Caching](https://turbo.build/repo/docs/core-concepts/caching)
- [Remote Caching](https://turbo.build/repo/docs/core-concepts/remote-caching)
- [Filtering](https://turbo.build/repo/docs/core-concepts/monorepos/filtering)
- [Configuration Options](https://turbo.build/repo/docs/reference/configuration)
- [CLI Usage](https://turbo.build/repo/docs/reference/command-line-reference)
