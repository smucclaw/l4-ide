{ haskell, ... }:
let
  hlib = haskell.lib.compose;
  hpkgs = haskell.packages.ghc98;
  jl4-websessions = hpkgs.callCabal2nix "jl4-websessions" ../../jl4-websessions { };
  # Don't use justStaticExecutables - it fails with GHC reference errors
  # due to Paths_* modules embedding store paths.
in
jl4-websessions
