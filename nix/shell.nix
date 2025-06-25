{
  pkgs ? import <nixpkgs> { },
  ...
}:
pkgs.mkShell {
  buildInputs = [
    pkgs.haskell.compiler.ghc98
    pkgs.haskell.packages.ghc98.haskell-language-server
    pkgs.cabal-install
    pkgs.ghciwatch
    pkgs.zlib
    pkgs.xz
    pkgs.pkg-config
    pkgs.nodejs
    pkgs.typescript
    pkgs.nixos-anywhere
    pkgs.sqlite
    pkgs.prettier
  ];
}
