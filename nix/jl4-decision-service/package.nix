{ haskell, ... }:
let
  hlib = haskell.lib.compose;
  hpkgs = haskell.packages.ghc910.override {
    overrides = import ../jl4-lsp/hs-overlay.nix hlib;
  };
  jl4-decision-service = hpkgs.callCabal2nix "jl4-decision-service" ../../jl4-decision-service { };
  # Don't use justStaticExecutables - it fails with GHC reference errors
  # due to Paths_* modules embedding store paths.
in
hlib.doJailbreak jl4-decision-service
