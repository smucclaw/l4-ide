{
  url,
  secure,
  buildNpmPackage,
  importNpmLock,
  pkg-config,
  libsecret,
  ...
}:
buildNpmPackage {
  pname = "jl4-web";
  version = "0-latest";
  src = ../../.;
  npmDeps = importNpmLock { npmRoot = ../../.; };
  npmWorkspace = ../../.;
  nativeBuildInputs = [ pkg-config ];
  buildInputs = [ libsecret.dev ];
  buildPhase = ''
    runHook preBuild
    export VITE_BACKEND_URL=${if secure then "wss" else "ws"}://${url};
    cd ./ts-apps/jl4-web
    npm run build
    runHook postBuild
  '';
  installPhase = ''
    mv build $out
  '';
  npmFlags = [ ];
  npmConfigHook = importNpmLock.npmConfigHook;
}
