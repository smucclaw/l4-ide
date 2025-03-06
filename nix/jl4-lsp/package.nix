{ haskell, ... }:
let
  hlib = haskell.lib.compose;
  hpkgs = haskell.packages.ghc98.override {
    overrides = import ./hs-overlay.nix;
  };
  jl4 = hpkgs.callCabal2nix "jl4" ../../jl4 { };
in
hlib.justStaticExecutables (hlib.doJailbreak jl4)
