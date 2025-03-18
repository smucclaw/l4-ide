{ haskell, ... }:
let
  hlib = haskell.lib.compose;
  hpkgs = haskell.packages.ghc98.override {
    overrides = import ./hs-overlay.nix hlib;
  };
in
hlib.justStaticExecutables hpkgs.jl4-lsp
