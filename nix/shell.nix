{
  pkgs ? import <nixpkgs> { },
  ...
}:
pkgs.mkShell {
  buildInputs = [
    pkgs.haskell.compiler.ghc98
    pkgs.haskell.packages.ghc98.haskell-language-server
    pkgs.cabal-install
    pkgs.zlib
    pkgs.nodejs
    pkgs.typescript
    pkgs.nixos-anywhere
    pkgs.sqlite
  ];
}
