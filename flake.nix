{
  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    disko.url = "github:nix-community/disko";
  };

  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" "x86_64-windows" ];
      perSystem =
        { pkgs, ... }:
        {
          packages = import ./nix/default.nix { inherit pkgs; };
          devShells.default = import ./nix/shell.nix { inherit pkgs; };
        };
      flake = {
        # initial prototype from WT
        nixosConfigurations.jl4-demo = inputs.nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            {
              system.stateVersion = "24.11"; #  do not change even if you change nixpkgs.url above.
              jl4-demo = {
                domain = "jl4.well-typed.com";
                acme-email = "magnus@well-typed.com";
                root-ssh-keys = [
                  "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEkSSWT6/5jBteJYcwiwltGp+hPZM/rQcqbOENvMvIj3 mangoiv@p14-nixos"
                  "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJnW5fx9PJGfn5x6EcYdl2XlGxOiZKWfzPgzLYqCn7zW hannes@well-typed.com"
                  "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAfJEsr9v7Dh6uCv/gXRMW7AKGcBSprx23SwxiMPcYQP andres@well-typed.com"
                ];
              };
            }
            inputs.disko.nixosModules.default
            ./nix/configuration.nix
            ./nix/hetzner.nix
          ];
        };

        # working version on AWS
        nixosConfigurations.jl4-aws-2505 = inputs.nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";

          modules = [
            (let stateVer = "25.05"; in {
              system.stateVersion = stateVer;
              # do not change the stateVersion even if you change nixpkgs.url above.
              # the system is not fully stateless because the jl4-websessions component persists l4 programs to sqlite
              jl4-demo = {
                domain = "jl4.legalese.com";
                acme-email = "mengwong@legalese.com";
                root-ssh-keys = [
                  "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIO2u9PR5FnBb8joGKHUVGqy9/cZu/iXIjgLpblkOF0H+ meng-and-ruslan"
                ];
              };
              environment.etc."my-deploy-marker" = {
                text = ''
                deployed-from-flake-2025-05-12 15:37
                system.stateVersion = ${stateVer}
                '';
                mode = "0444";
              };
                 })
            inputs.disko.nixosModules.default
            ./nix/configuration.nix
            ./nix/aws-ec2.nix
            ./nix/aws-vm.nix
          ];
        };
      };
    };
}
