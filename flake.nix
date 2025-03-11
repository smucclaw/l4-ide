{
  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    disko.url = "github:nix-community/disko";
  };

  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ];
      perSystem =
        { pkgs, ... }:
        {
          packages = import ./nix/default.nix { inherit pkgs; };
          devShells.default = import ./nix/shell.nix { inherit pkgs; };
        };
      flake = {
        nixosConfigurations.jl4-demo = inputs.nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            {
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
      };
    };
}
