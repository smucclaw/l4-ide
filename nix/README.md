# nix support

## redeploying

Assuming you have already deployed using the instructions below,

Deployed at WT:

    nixos-rebuild switch --flake .#jl4-demo --target-host root@olive.well-typed.com

Deployed at Legalese:

    nixos-rebuild switch --flake '.#jl4-aws-2505' --target-host root@nano

This tends to throw a systemd network service error but the deployed service actually comes up fine.

## deploying to a new machine on Hetzner

If the machine is an x86_64 cloud machine on hetzner, then just run

```sh
nixos-anywhere --flake .#jl4-demo --target-host root@olive.well-typed.com
```

`nixos-anywhere` is in `nixpkgs` and can be run directly from repo's flake.

If you're deploying to a different machine type, create a new file like `hetzner.nix`, then
create a new output in `flake.nixosModules` in the flake and import your new module as well
as the application specific modules from there. Then use `nixos-anywhere` with the respective
`target-host`.

## deploying to a new machine on AWS EC2

Following the instructions above:

Suppose you have an ubuntu t3a.small instance on EC2

Assuming you have preconfigured your .ssh/config with the SSH key to log in without password,

    % nixos-anywhere --flake '.#jl4-aws' ubuntu@ecXXXXXX.ap-southeast-1.compute.amazonaws.com

You can then reach the web interface at the EC2 hostname, and the decision service under `/decision/swagger-ui/` and so on, all served by Nginx.

The ACME letsencrypt component should come up fine.

After you bring this up on the small instance you can reset the instance to nano and everything should run fine.

## deploying to a new VM for testing purposes

On a NixOS machine, the `aws-vm.nix` configuration allows you to
preview the deployment inside a VM which you can bring up locally.
This variant is automatically run when you do

    nixos-rebuild build-vm --flake '.#jl4-aws-2505'

You can then run that VM with

    result/bin/run-jl4-demo-vm

## Persistence

The UUID store run by jl4-websessions will be wiped if you do a brand new deployment.

## See Also

Check the [documentation for `nixos-anywhere`](https://github.com/nix-community/nixos-anywhere)

and the example `hetzner.nix` to figure out what you need to do.
