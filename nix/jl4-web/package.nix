{
  url,
  secure,
  buildNpmPackage,
  importNpmLock,
  ...
}:
buildNpmPackage {
  pname = "jl4-web";
  version = "0-latest";
  src = ../../ts-apps/jl4-web;
  npmDeps = importNpmLock {
    npmRoot = ../../.;
    package = builtins.fromJSON (builtins.readFile ../../ts-apps/jl4-web/package.json);
  };
  buildPhase = ''
    runHook preBuild
    export VITE_BACKEND_URL=${if secure then "wss" else "ws"}://${url};
    npm run build
    runHook postBuild
  '';
  installPhase = ''
    mv dist $out
  '';
  npmConfigHook = importNpmLock.npmConfigHook;
}
