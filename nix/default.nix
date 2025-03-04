{
  pkgs ? import <nixpkgs> { },
  ...
}:
{
  jl4-web = pkgs.callPackage ./jl4-web/package.nix {
    url = "localhost";
    secure = false;
  };
  jl4-lsp = pkgs.callPackage ./jl4-lsp/package.nix { };
}
