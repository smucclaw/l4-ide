{haskell, ...}: let 
  hlib = haskell.lib.compose;
  hpkgs = haskell.packages.ghc98.override {
    overrides = hself: hsuper: {
      megaparsec = hsuper.callHackage "megaparsec" "9.7.0" {};
      text-rope = hsuper.callHackage "text-rope" "0.3" {};
    };
  };
  jl4 = hpkgs.callCabal2nix "jl4" ../../jl4 {};
in hlib.justStaticExecutables (hlib.doJailbreak jl4)
