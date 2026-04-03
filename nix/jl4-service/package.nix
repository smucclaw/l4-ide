{ haskell, ... }:
let
  hlib = haskell.lib.compose;
  hpkgs = haskell.packages.ghc910.override {
    overrides = import ../jl4-lsp/hs-overlay.nix hlib;
  };
  jl4-service = hpkgs.callCabal2nix "jl4-service" ../../jl4-service { };
  # Don't use justStaticExecutables - it fails with GHC reference errors
  # due to Paths_* modules embedding store paths.
in
hlib.doJailbreak jl4-service
