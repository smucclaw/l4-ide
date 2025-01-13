# README

## Installation for contributors

Install node.js (>= 18) if you don't already have it.

Then do the following from `typescript-monorepo`.

To ensure that we're using the same version of the same package manager

- Enable corepack with `corepack enable`.

- Then `corepack install` to install the package manager used by this monorepo.

Then `npm install`, again from `typescript-monorepo`.

Finally, you'll want to install `turbo` _both_ globally and locally.

- To install it _globally_: `npm install turbo --global` or `npm install turbo --global`.

(See https://turbo.build/repo/docs/getting-started/installation if you need more help with turbo installation.)

Once the dependencies have been installed,
the TS projects can be built by running `turbo build`
from the `typescript-monorepo` dir.

## Key scripts

- To lint: `turbo lint`
- To format with Prettier: `npm format` (TODO: May want to make this a Turbo task instead)
  - There is also a VSCode task for formatting the monorepo.
- To develop all apps and packages: `turbo dev`

### Further notes on setup / build process / configs for developers of the TS monorepo

#### VSCode extension build related

Right now we use esbuild (see the package.json in apps/vscode), 
but this needs to be looked at more / improved.

Something worth noting is that

`npm run esbuild-base`

was required for me to not get an error about how vscode-languageclient/node could not be found when starting the extension in VSCode.

**TODO**: There is also a hack-y attempt in .vscodeignore
to stop `vsce` from trying to bundle files from ancestor directories. The way to
improve this might be to improve the bundling situation.

See

* https://github.com/microsoft/vscode-vsce/issues/777
* https://github.com/eclipse-langium/langium/pull/1520

#### Prettier and ESLint

- There are shared `eslint` and `prettier` configs in `./shared`.
- ESLint: We're using the flat config file format.
- Prettier: Not sure yet what should be done in the sub-package/app package.json vs. the top-level package.json. But for now, you can `npm format` from `mattwaddington/typescript-monorepo` as well as from the vscode extension directory.
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
