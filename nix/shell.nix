{
  pkgs ? import <nixpkgs> { },
  ...
}:
pkgs.mkShell {
  buildInputs = [
    pkgs.haskell.compiler.ghc98
    pkgs.cabal-install
    pkgs.ghciwatch
    pkgs.zlib
    pkgs.xz
    pkgs.pkg-config
    pkgs.nodejs
    pkgs.typescript
    pkgs.nixos-anywhere
    pkgs.sqlite
    pkgs.nodePackages.prettier
    pkgs.graphviz
    pkgs.xdot
  ];
}
