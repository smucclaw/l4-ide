#!/bin/bash
echo "Building jl4-lsp for x86_64-unknown-linux-gnu"
echo "This requires cross-compilation setup or native linux machine"
echo ""
echo "Commands to run:"
echo "cabal build exe:jl4-lsp"
echo "cp $(cabal list-bin exe:jl4-lsp) jl4-lsp"
echo "strip jl4-lsp"
echo "chmod +x jl4-lsp"
