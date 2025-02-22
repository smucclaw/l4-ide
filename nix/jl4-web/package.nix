{buildNpmPackage, importNpmLock, ...}: buildNpmPackage {
  pname = "jl4-web";
  version = "0-latest";
  src = ../../ts-apps/jl4-web;
  npmDeps = importNpmLock {
    npmRoot = ../../.;
    package = builtins.fromJSON (builtins.readFile ../../ts-apps/jl4-web/package.json);
  };
  installPhase = ''
    mv dist $out
  '';
  npmConfigHook = importNpmLock.npmConfigHook;
}

