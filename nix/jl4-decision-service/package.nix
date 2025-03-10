{ haskell, lib, ... }:
let
  hlib = haskell.lib.compose;
  hpkgs = haskell.packages.ghc98.override {
    overrides = lib.composeExtensions
      (import ../jl4-lsp/hs-overlay.nix)
      (hself: _: { jl4 = hlib.doJailbreak (hself.callCabal2nix "jl4" ../../jl4 {}); })
      ;
  };
  jl4-decision-service = hpkgs.callCabal2nix "jl4-decision-service" ../../jl4-decision-service { };
in
hlib.justStaticExecutables (hlib.doJailbreak jl4-decision-service)
