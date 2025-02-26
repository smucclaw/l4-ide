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
    npm run build || true
    # HACK: we need to build workspace dependencies but we don't care if
    # not all of them succeed as long as enough succeed for the entire
    # thing to build
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
