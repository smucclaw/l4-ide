{
  pkgs ? import <nixpkgs> { },
  ...
}:
{
  jl4-web = pkgs.callPackage ./jl4-web/package.nix { };
  jl4-lsp = pkgs.callPackage ./jl4-lsp/package.nix { };
  jl4-websessions = pkgs.callPackage ./jl4-websessions/package.nix { };
  jl4-decision-service = pkgs.callPackage ./jl4-decision-service/package.nix { };
}
