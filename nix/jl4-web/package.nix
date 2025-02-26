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
    set -x
    export VITE_BACKEND_URL=${if secure then "wss" else "ws"}://${url};
    pushd ./ts-shared/viz-expr
    npm run build
    popd
    pushd ./ts-apps
    pushd ./decision-logic-visualizer
    npm run build
    popd

    pushd ./webview
    npm run build
    popd

    # HACK: we need to build workspace dependencies by hand because npm
    # isn't clever enough to figure out workspace dependencies

    pushd ./jl4-web
    npm run build
    set +x
    runHook postBuild
  '';
  installPhase = ''
    mv build $out
  '';
  npmFlags = [ ];
  npmConfigHook = importNpmLock.npmConfigHook;
}
