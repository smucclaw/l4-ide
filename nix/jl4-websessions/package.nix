{ haskell, ... }:
let
  hlib = haskell.lib.compose;
  hpkgs = haskell.packages.ghc910;
  jl4-websessions = hpkgs.callCabal2nix "jl4-websessions" ../../jl4-websessions { };
in
hlib.justStaticExecutables jl4-websessions
