# nix support

## redeploying to aws (sg) production

```sh
mengwong@lhs:~/src/smucclaw/l4-ide  main! 2025-09-15 11:46:32
% nixos-rebuild switch --flake '.#jl4-aws-2505' --target-host nano
```

## redeploying to hetzner (eu) development

```
nixos-rebuild switch --flake .#jl4-demo --target-host root@example.com
```

## deploying to a new machine

If the machine is an x86_64 cloud machine on hetzner, then just run

```sh
nixos-anywhere --flake .#jl4-demo --target-host root@example.com
```

`nixos-anywhere` is in `nixpkgs` and can be run directly from repo's flake.

If you're deploying to a different machine type, create a new file like `hetzner.nix`, then
create a new output in `flake.nixosModules` in the flake and import your new module as well
as the application specific modules from there. Then use `nixos-anywhere` with the respective
`target-host`.

Check the [documentation for `nixos-anywhere`](https://github.com/nix-community/nixos-anywhere)
and the example `hetzner.nix` to figure out what you need to do.

## transferring system and application state to a new machine

- /var/lib/acme/
- /var/lib/private/jl4-websessions/
