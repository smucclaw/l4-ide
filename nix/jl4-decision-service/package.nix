{ haskell, ... }:
let
  hlib = haskell.lib.compose;
  hpkgs = haskell.packages.ghc98.override {
    overrides = import ../jl4-lsp/hs-overlay.nix hlib;
  };
  jl4-decision-service = hpkgs.callCabal2nix "jl4-decision-service" ../../jl4-decision-service {  };
in
hlib.doJailbreak (hlib.overrideCabal jl4-decision-service (oldAttrs: {
  postInstall = ''
    # Create data directory if it doesn't exist
    mkdir -p $out/share/jl4
    mkdir -p $out/share/doc
    
    # Copy jl4/experiments directory
    if [ -d ../../jl4/experiments ]; then
      cp -r ../../jl4/experiments $out/share/jl4/
    else
      echo "Warning: ../../jl4/experiments directory not found"
    fi
    
    # Copy doc/tutorial-code directory
    if [ -d ../../doc/tutorial-code ]; then
      cp -r ../../doc/tutorial-code $out/share/doc/
    else
      echo "Warning: ../../doc/tutorial-code directory not found"
    fi
  '';
}))
