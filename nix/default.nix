{pkgs ? import <nixpkgs> {}, ...}: {
  jl4-web =  pkgs.callPackage ./jl4-web/package.nix {};
  jl4-lsp = pkgs.callPackage ./jl4-lsp/package.nix {};
}
