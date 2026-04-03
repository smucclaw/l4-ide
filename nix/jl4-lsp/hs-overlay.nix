hlib: hself: hsuper: {
  megaparsec = hsuper.callHackage "megaparsec" "9.7.0" { };
  text-rope = hsuper.callHackage "text-rope" "0.3" { };
  # chronos 1.1.7 requires text >=2.1.2 && <2.2, jailbreak to allow newer text versions
  chronos = hlib.doJailbreak hsuper.chronos;
  # graphviz has a flaky property test ("Ensure augmentation is valid" fails intermittently)
  graphviz = hlib.dontCheck hsuper.graphviz;
  jl4-websessions = hlib.doJailbreak (hself.callCabal2nix "jl4-websessions" ../../jl4-websessions { });
  jl4-core = hlib.doJailbreak (hself.callCabal2nix "jl4-core" ../../jl4-core { });
  jl4-query-plan = hlib.doJailbreak (hself.callCabal2nix "jl4-query-plan" ../../jl4-query-plan { });
  jl4-lsp = hlib.doJailbreak (hself.callCabal2nix "jl4-lsp" ../../jl4-lsp { });
}
