{ haskell, lib, removeReferencesTo, ... }:
let
  hlib = haskell.lib.compose;
  hpkgs = haskell.packages.ghc98.override {
    overrides = import ./hs-overlay.nix hlib;
  };
  ghc = hpkgs.ghc;
  # Build the executable with justStaticExecutables, but override disallowedReferences
  # to allow GHC references that may come from Paths_jl4_core module.
  # The Paths module embeds data directory paths which can transitively reference GHC.
  pkg = hlib.justStaticExecutables hpkgs.jl4-lsp;
in
pkg.overrideAttrs (old: {
  # Remove the disallowedReferences check for GHC since Paths_jl4_core may embed paths
  disallowedReferences = lib.filter (ref: ref != ghc) (old.disallowedReferences or []);
  # Explicitly remove references to GHC from the binary
  postFixup = (old.postFixup or "") + ''
    ${removeReferencesTo}/bin/remove-references-to -t ${ghc} $out/bin/jl4-lsp
  '';
})
