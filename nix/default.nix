{
  pkgs ? import <nixpkgs> { },
  ...
}:
let
  # Support cross-compilation
  crossSystems = {
    "x86_64-linux" = "x86_64-unknown-linux-gnu";
    "x86_64-darwin" = "x86_64-apple-darwin";
    "aarch64-darwin" = "aarch64-apple-darwin";
    "x86_64-windows" = "x86_64-pc-windows-msvc";
  };
  
  # Create cross-compiled packages for each system
  crossPackages = pkgs.lib.mapAttrs (system: target: {
    jl4-lsp = pkgs.pkgsCross.${system}.callPackage ./jl4-lsp/package.nix { };
  }) crossSystems;
in
{
  jl4-web = pkgs.callPackage ./jl4-web/package.nix { };
  jl4-lsp = pkgs.callPackage ./jl4-lsp/package.nix { };
  jl4-websessions = pkgs.callPackage ./jl4-websessions/package.nix { };
  jl4-decision-service = pkgs.callPackage ./jl4-decision-service/package.nix { };
} // crossPackages
