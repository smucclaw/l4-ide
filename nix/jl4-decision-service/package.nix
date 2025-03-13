{ haskell, ... }:
let
  hlib = haskell.lib.compose;
  hpkgs = haskell.packages.ghc98.override {
    overrides = import ../jl4-lsp/hs-overlay.nix hlib;
  };
  jl4-decision-service = hpkgs.callCabal2nix "jl4-decision-service" ../../jl4-decision-service { };
in
hlib.justStaticExecutables (hlib.doJailbreak jl4-decision-service)
