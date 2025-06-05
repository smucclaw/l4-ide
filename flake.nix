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
                domain = "jl4.ymhan.com";
                acme-email = "hanyongming@gmail.com";
                root-ssh-keys = [
                  "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJMbG7vxqdq3eJDVoGmPw7CIxv132Z9ueeU4X2D/VxJP hanyongming@gmail.com"
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
