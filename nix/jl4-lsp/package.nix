{ haskell, ... }:
let
  hlib = haskell.lib.compose;
  hpkgs = haskell.packages.ghc910.override {
    overrides = import ./hs-overlay.nix hlib;
  };
in
hlib.justStaticExecutables hpkgs.jl4-lsp
