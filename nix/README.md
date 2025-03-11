# nix support

## redeploying

```sh
nixos-rebuild switch --flake .#jl4-demo --target-host root@olive.well-typed.com
```

## deploying to a new machine

If the machine is an x86_64 cloud machine on hetzner, then just run

```sh
nixos-anywhere --flake .#jl4-demo --target-host root@olive.well-typed.com
```

`nixos-anywhere` is in `nixpkgs` and can be run directly from repo's flake.

If you're deploying to a different machine type, create a new file like `hetzner.nix`, then
create a new output in `flake.nixosModules` in the flake and import your new module as well
as the application specific modules from there. Then use `nixos-anywhere` with the respective
`target-host`.

Check the [documentation for `nixos-anywhere`](https://github.com/nix-community/nixos-anywhere)
and the example `hetzner.nix` to figure out what you need to do.
