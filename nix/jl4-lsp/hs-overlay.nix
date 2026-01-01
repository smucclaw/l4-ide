hlib: hself: hsuper: {
  megaparsec = hsuper.callHackage "megaparsec" "9.7.0" { };
  text-rope = hsuper.callHackage "text-rope" "0.3" { };
  jl4-websessions = hlib.doJailbreak (hself.callCabal2nix "jl4-websessions" ../../jl4-websessions { });
  jl4-core = hlib.doJailbreak (hself.callCabal2nix "jl4-core" ../../jl4-core { });
  jl4-query-plan = hlib.doJailbreak (hself.callCabal2nix "jl4-query-plan" ../../jl4-query-plan { });
  jl4-lsp = hlib.doJailbreak (hself.callCabal2nix "jl4-lsp" ../../jl4-lsp { });
}
