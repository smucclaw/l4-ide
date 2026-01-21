{ haskell, ... }:
let
  hlib = haskell.lib.compose;
  hpkgs = haskell.packages.ghc98.override {
    overrides = import ./hs-overlay.nix hlib;
  };
  # Don't use justStaticExecutables since Paths_jl4_core embeds data directory paths
  # that can transitively reference GHC. The closure will be larger but the build
  # will succeed. The service config already specifies --cwd for library files.
in
hpkgs.jl4-lsp
