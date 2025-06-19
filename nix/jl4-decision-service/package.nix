{ haskell, lib, runCommand, ... }:
let
  hlib = haskell.lib.compose;
  hpkgs = haskell.packages.ghc98.override {
    overrides = import ../jl4-lsp/hs-overlay.nix hlib;
  };
  
  # Path to the root of the source tree
  sourceRoot = ../..;
  
  # Create a source directory that includes the data files
  sourceWithData = runCommand "jl4-decision-service-with-data" {} ''
    # Copy the main source directory
    cp -r ${sourceRoot}/jl4-decision-service $out
    chmod -R u+w $out
    
    # Create the data directories to match the data-files paths in the .cabal file
    mkdir -p $out/jl4
    mkdir -p $out/doc
    
    # Copy the required data files to match the data-files paths in the .cabal file
    if [ -d ${sourceRoot}/jl4/experiments ]; then
      cp -r ${sourceRoot}/jl4/experiments $out/jl4/
    else
      echo "Warning: jl4/experiments directory not found"
    fi
    
    if [ -d ${sourceRoot}/doc/tutorial-code ]; then
      cp -r ${sourceRoot}/doc/tutorial-code $out/doc/
    else
      echo "Warning: doc/tutorial-code directory not found"
    fi
  '';
  
  customSource = hpkgs.callCabal2nix "jl4-decision-service" sourceWithData { };
  
  jl4-decision-service = customSource;
in
hlib.doJailbreak jl4-decision-service
